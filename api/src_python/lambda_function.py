"""
AWS Lambda handler for the DiAGRAM API backend.

Routes API-Gateway proxy-format events to the appropriate handler function
and returns API-Gateway-compatible responses.

Routes:
  POST /api/test/is_alive          – health check
  POST /api/model/score            – Bayesian-network scoring
  POST /api/chart/plot             – bar-chart PNG
  POST /api/report/pdf             – PDF report
  POST /api/report/csv             – CSV report
  POST /api/validation/validate_json – JSON validation
"""

import base64
import json
import logging
import os
import traceback

logger = logging.getLogger()
logger.setLevel(logging.INFO)

# ---------------------------------------------------------------------------
# Lazy imports – keep them at module level so Lambda warm starts benefit
# from already-loaded modules, but fail-fast only on actual invocation.
# ---------------------------------------------------------------------------
from model import load_default_model, score_model_
from responses import extract_responses
from validate import validate
from plot import write_temp_png
from pdf_report import write_temp_pdf
from csv_report import build_csv

# Pre-load the Bayesian network at cold start (expensive operation).
try:
    _MODEL = load_default_model()
    logger.info("Default model loaded at cold start.")
except Exception as exc:
    logger.error("Failed to load default model: %s", exc)
    _MODEL = None


# ---------------------------------------------------------------------------
# Response helpers
# ---------------------------------------------------------------------------

def _json_response(body: dict, status: int = 200) -> dict:
    return {
        "statusCode": status,
        "isBase64Encoded": False,
        "headers": {"Content-Type": "application/json"},
        "body": json.dumps(body),
    }


def _binary_response(data: bytes, content_type: str, status: int = 200) -> dict:
    return {
        "statusCode": status,
        "isBase64Encoded": True,
        "headers": {"Content-Type": content_type},
        "body": base64.b64encode(data).decode("ascii"),
    }


def _csv_response(csv_text: str, status: int = 200) -> dict:
    return {
        "statusCode": status,
        "isBase64Encoded": False,
        "headers": {"Content-Type": "text/csv; charset=UTF-8"},
        "body": csv_text,
    }


def _error_response(message: str, status: int = 400) -> dict:
    return _json_response({"error": message}, status=status)


# ---------------------------------------------------------------------------
# Route handlers
# ---------------------------------------------------------------------------

def _handle_is_alive() -> dict:
    return _json_response({"alive": True})


def _handle_score_model(body_str: str) -> dict:
    parsed = json.loads(body_str)
    if isinstance(parsed, list):
        if len(parsed) != 1:
            return _error_response("Expected a single JSON object (or array of one).")
        parsed = parsed[0]

    # Wrap as a list for extract_responses then take the first result
    responses = extract_responses([parsed])
    if not responses:
        return _error_response("No valid responses found in request body.")

    score = score_model_(responses[0], _MODEL)
    return _json_response(score)


def _handle_chart(body_str: str) -> dict:
    data = json.loads(body_str)
    if not isinstance(data, list):
        data = [data]

    tmp_path = write_temp_png(data)
    try:
        with open(tmp_path, "rb") as fh:
            png_bytes = fh.read()
    finally:
        try:
            os.unlink(tmp_path)
        except OSError:
            pass

    return _binary_response(png_bytes, "image/png")


def _handle_pdf(body_str: str) -> dict:
    parsed = json.loads(body_str)
    if not isinstance(parsed, list):
        parsed = [parsed]

    responses = extract_responses(parsed)

    tmp_path = write_temp_pdf(responses)
    try:
        with open(tmp_path, "rb") as fh:
            pdf_bytes = fh.read()
    finally:
        try:
            os.unlink(tmp_path)
        except OSError:
            pass

    return _binary_response(pdf_bytes, "application/pdf")


def _handle_csv(body_str: str) -> dict:
    parsed = json.loads(body_str)
    csv_text = build_csv(parsed)
    return _csv_response(csv_text)


def _handle_validate(body_str: str) -> dict:
    parsed = json.loads(body_str)
    valid = validate(parsed)
    return _json_response({"status": valid})


# ---------------------------------------------------------------------------
# Route table
# ---------------------------------------------------------------------------

_ROUTES = {
    "test/is_alive": lambda _body: _handle_is_alive(),
    "model/score": _handle_score_model,
    "chart/plot": _handle_chart,
    "report/pdf": _handle_pdf,
    "report/csv": _handle_csv,
    "validation/validate_json": _handle_validate,
}


# ---------------------------------------------------------------------------
# Main handler
# ---------------------------------------------------------------------------

def handler(event, context):
    """AWS Lambda entry point."""
    try:
        # Decode body
        body = event.get("body") or ""
        if event.get("isBase64Encoded", False) and body:
            body = base64.b64decode(body).decode("utf-8")

        # Extract route: strip leading slash and /api/ prefix
        raw_path = event.get("path", "") or event.get("rawPath", "")
        route = raw_path.lstrip("/")
        if route.startswith("api/"):
            route = route[4:]
        route = route.rstrip("/")

        logger.info("Routing request: %s", route)

        handler_fn = _ROUTES.get(route)
        if handler_fn is None:
            return _error_response(f"Unknown route: /{route}", status=404)

        return handler_fn(body)

    except json.JSONDecodeError as exc:
        logger.warning("JSON decode error: %s", exc)
        return _error_response(f"Invalid JSON: {exc}", status=400)

    except Exception as exc:
        logger.error("Unhandled error: %s\n%s", exc, traceback.format_exc())
        return _error_response(f"Internal server error: {exc}", status=500)
