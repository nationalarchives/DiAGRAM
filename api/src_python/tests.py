"""
Python unit tests for the DiAGRAM API, mirroring the R testthat suite in
api/src/tests/testthat/.

Test files covered:
  test-model.R         → TestModel
  test-responses.R     → TestToNumeric, TestToProbability
  test-validation.R    → TestValidation
  test-csv.R           → TestCsv
  test-plots.R         → TestPlots
  test-pdf.R           → TestPdf
  test-api.R           → TestLambdaHandler

Run with:
  cd api/src_python
  python -m pytest tests.py -v
"""

import base64
import io
import json
import os
import sys
import unittest

# Allow imports from the src_python directory
sys.path.insert(0, os.path.dirname(__file__))

from bn_inference import parse_bif
from csv_report import build_csv
from lambda_function import handler
from model import load_default_model, score_model_
from nodes import USER_NODES, MODEL_PROB_NAMES
from pdf_report import generate_pdf
from plot import _prepare_chart_data, render_chart_to_bytes
from responses import extract_responses, advanced_flags
from to_numeric import to_numeric_simple
from to_probability import numeric_to_probability_simple
from validate import validate

# ── Test data helpers ────────────────────────────────────────────────────────

_TEST_DATA = os.path.join(
    os.path.dirname(__file__), "..", "src", "inst", "extdata", "test_data"
)


def _load(subdir: str, name: str) -> dict:
    path = os.path.join(_TEST_DATA, subdir, f"{name}_model.json")
    with open(path) as f:
        return json.load(f)


def _score_req(name: str) -> dict:
    return _load("score_requests", name)


def _plot_req(name: str) -> dict:
    return _load("plot_requests", name)


def _csv_req(name: str) -> dict:
    return _load("csv_requests", name)


def _pdf_req(name: str) -> list:
    """Load a PDF request fixture.

    The pdf_requests files are full API Gateway events whose ``body`` field
    contains an embedded JSON string (with possible control characters from
    R's serialiser).  Extract and return the parsed body list.
    """
    path = os.path.join(_TEST_DATA, "pdf_requests", f"{name}_model.json")
    with open(path) as f:
        gateway_event = json.loads(f.read(), strict=False)
    return json.loads(gateway_event["body"], strict=False)


def _lambda_event(path: str, body) -> dict:
    return {
        "path": path,
        "body": json.dumps(body) if not isinstance(body, str) else body,
        "isBase64Encoded": False,
    }


# ── Shared fixtures ──────────────────────────────────────────────────────────

_BASE_MODEL = None


def _base_model():
    global _BASE_MODEL
    if _BASE_MODEL is None:
        _BASE_MODEL = load_default_model()
    return _BASE_MODEL


def _simple_responses():
    data = _score_req("simple_single")
    return extract_responses(data)[0]


def _advanced_responses():
    """Return the advanced entry from mixed_model.json (is_advanced=True)."""
    data = _score_req("mixed")
    return next(r for r in extract_responses(data) if r["type"] == "advanced_responses")


# ════════════════════════════════════════════════════════════════════════════
# test-model.R
# ════════════════════════════════════════════════════════════════════════════

class TestModel(unittest.TestCase):

    def test_default_model_loads(self):
        """load_default_model() returns a BayesianNetwork with 21 nodes."""
        model = load_default_model()
        self.assertEqual(len(model.nodes()), 21)

    def test_model_has_all_user_nodes(self):
        """All user-editable nodes are present in the loaded model."""
        model = load_default_model()
        for node in USER_NODES:
            self.assertIn(node, model.nodes())

    def test_score_model_simple_returns_correct_keys(self):
        """score_model_() for a simple request returns the expected keys."""
        responses = _simple_responses()
        score = score_model_(responses, _base_model())
        self.assertIsInstance(score, dict)
        self.assertIn("intellectual_control", score)
        self.assertIn("renderability", score)
        self.assertIn("nodes", score)

    def test_score_model_simple_scores_in_range(self):
        """Scores are in the 0–100 range."""
        responses = _simple_responses()
        score = score_model_(responses, _base_model())
        self.assertGreaterEqual(score["intellectual_control"], 0)
        self.assertLessEqual(score["intellectual_control"], 100)
        self.assertGreaterEqual(score["renderability"], 0)
        self.assertLessEqual(score["renderability"], 100)

    def test_score_model_advanced(self):
        """score_model_() for an advanced request returns the expected keys."""
        responses = _advanced_responses()
        score = score_model_(responses, _base_model())
        self.assertIsInstance(score, dict)
        self.assertIn("intellectual_control", score)
        self.assertIn("renderability", score)
        self.assertIn("nodes", score)

    def test_score_model_regression_simple(self):
        """Simple model IC=0.0, R=44.679 (regression against R output)."""
        data = _score_req("simple")
        responses = extract_responses(data)[0]
        score = score_model_(responses, _base_model())
        self.assertAlmostEqual(score["intellectual_control"], 0.0, places=2)
        self.assertAlmostEqual(score["renderability"], 44.679, places=2)

    def test_score_model_regression_advanced(self):
        """Advanced model IC=32.0139, R=24.2141 (regression against R output)."""
        responses = _advanced_responses()
        score = score_model_(responses, _base_model())
        self.assertAlmostEqual(score["intellectual_control"], 32.0139, places=2)
        self.assertAlmostEqual(score["renderability"], 24.2141, places=2)

    def test_score_model_nodes_contains_all_nodes(self):
        """The nodes dict in the score result covers all 21 BN nodes."""
        responses = _simple_responses()
        score = score_model_(responses, _base_model())
        self.assertEqual(len(score["nodes"]), 21)

    def test_model_is_not_mutated_between_calls(self):
        """Scoring does not mutate the base model — results are reproducible."""
        data = _score_req("simple")
        responses = extract_responses(data)[0]
        model = load_default_model()
        score1 = score_model_(responses, model)
        score2 = score_model_(responses, model)
        self.assertEqual(score1["intellectual_control"], score2["intellectual_control"])
        self.assertEqual(score1["renderability"], score2["renderability"])


# ════════════════════════════════════════════════════════════════════════════
# test-responses.R  (to_numeric + to_probability sections)
# ════════════════════════════════════════════════════════════════════════════

class TestToNumeric(unittest.TestCase):
    """Mirrors the individual node conversion tests in test-responses.R."""

    def setUp(self):
        self.responses = _simple_responses()
        self.response_data = self.responses["data"]["response"]

    def test_technical_skills_returns_numeric(self):
        val = to_numeric_simple(self.response_data)["Technical_Skills"]
        self.assertIsInstance(val, (int, float))

    def test_physical_disaster_returns_numeric(self):
        val = to_numeric_simple(self.response_data)["Physical_Disaster"]
        self.assertIsInstance(val, (int, float))

    def test_system_security_returns_numeric(self):
        val = to_numeric_simple(self.response_data)["System_Security"]
        self.assertIsInstance(val, (int, float))

    def test_checksum_returns_three_element_list(self):
        val = to_numeric_simple(self.response_data)["Checksum"]
        self.assertIsInstance(val, list)
        self.assertEqual(len(val), 3)
        for v in val:
            self.assertIsInstance(v, (int, float))

    def test_digital_object_returns_three_element_list(self):
        val = to_numeric_simple(self.response_data)["Digital_Object"]
        self.assertIsInstance(val, list)
        self.assertEqual(len(val), 3)

    def test_storage_medium_returns_three_element_list(self):
        val = to_numeric_simple(self.response_data)["Storage_Medium"]
        self.assertIsInstance(val, list)
        self.assertEqual(len(val), 3)

    def test_info_management_returns_numeric(self):
        val = to_numeric_simple(self.response_data)["Info_Management"]
        self.assertIsInstance(val, (int, float))

    def test_op_environment_returns_numeric(self):
        val = to_numeric_simple(self.response_data)["Op_Environment"]
        self.assertIsInstance(val, (int, float))

    def test_op_environment_offsite_gives_100(self):
        """When second question is 'Yes', Op_Environment should be 100."""
        res = {"1": 50, "2": "Yes"}
        val = to_numeric_simple({"Op_Environment": res})["Op_Environment"]
        self.assertEqual(val, 100)

    def test_rep_and_refresh_returns_numeric(self):
        val = to_numeric_simple(self.response_data)["Rep_and_Refresh"]
        self.assertIsInstance(val, (int, float))

    def test_full_conversion_returns_nine_nodes(self):
        """to_numeric_simple() returns exactly one value per user node."""
        result = to_numeric_simple(self.response_data)
        self.assertEqual(len(result), len(USER_NODES))
        self.assertEqual(set(result.keys()), set(USER_NODES))


class TestToProbability(unittest.TestCase):
    """Mirrors the numeric_to_probability tests in test-responses.R."""

    def setUp(self):
        self.responses = _simple_responses()
        self.response_data = self.responses["data"]["response"]
        self.numeric = to_numeric_simple(self.response_data)

    def test_scalar_node_produces_valid_distribution(self):
        """A two-state node factor sums to 1 with values in [0, 1]."""
        cpds = numeric_to_probability_simple({"Op_Environment": 50}, _base_model())
        factor = cpds["Op_Environment"]
        total = sum(factor.values.values())
        self.assertAlmostEqual(total, 1.0, places=6)
        for p in factor.values.values():
            self.assertGreaterEqual(p, 0.0)
            self.assertLessEqual(p, 1.0)

    def test_three_state_node_produces_valid_distribution(self):
        """A three-state node factor sums to 1."""
        cpds = numeric_to_probability_simple(
            {"Digital_Object": [10, 20, 70]}, _base_model()
        )
        factor = cpds["Digital_Object"]
        total = sum(factor.values.values())
        self.assertAlmostEqual(total, 1.0, places=6)

    def test_all_nodes_produce_valid_distributions(self):
        """All user-node factors are valid probability distributions."""
        cpds = numeric_to_probability_simple(self.numeric, _base_model())
        self.assertEqual(len(cpds), len(USER_NODES))
        for node, factor in cpds.items():
            total = sum(factor.values.values())
            self.assertAlmostEqual(total, 1.0, places=6,
                                   msg=f"Factor for {node} does not sum to 1")

    def test_advanced_flags_false_for_simple_request(self):
        """advanced_flags() contains no True entries for a simple request."""
        data = _score_req("simple_single")
        self.assertFalse(any(advanced_flags(data)))


# ════════════════════════════════════════════════════════════════════════════
# test-validation.R
# ════════════════════════════════════════════════════════════════════════════

class TestValidation(unittest.TestCase):

    def setUp(self):
        self.valid_data = _csv_req("mixed")

    def _event(self, body):
        return _lambda_event("/api/validation/validate_json", body)

    def test_valid_json_is_accepted(self):
        result = validate(self.valid_data)
        self.assertTrue(result)

    def test_mixed_simple_requires_is_advanced_key(self):
        """mixed_simple data lacks is_advanced key — Python validator correctly rejects it."""
        data = _csv_req("mixed_simple")
        self.assertFalse(validate(data))

    def test_invalid_without_model_name(self):
        """A response missing model_name should fail validation."""
        bad = [dict(self.valid_data[0])]
        del bad[0]["model_name"]
        self.assertFalse(validate(bad))

    def test_invalid_without_scenario(self):
        bad = [dict(self.valid_data[0])]
        del bad[0]["scenario"]
        self.assertFalse(validate(bad))

    def test_invalid_without_response(self):
        """A response missing the 'response' sub-object should fail validation."""
        bad = [dict(self.valid_data[0])]
        bad[0].pop("response", None)
        self.assertFalse(validate(bad))

    def test_lambda_endpoint_returns_200(self):
        event = self._event(self.valid_data)
        resp = handler(event, {})
        self.assertEqual(resp["statusCode"], 200)

    def test_lambda_endpoint_returns_json(self):
        event = self._event(self.valid_data)
        resp = handler(event, {})
        body = json.loads(resp["body"])
        self.assertIn("status", body)


# ════════════════════════════════════════════════════════════════════════════
# test-csv.R
# ════════════════════════════════════════════════════════════════════════════

class TestCsv(unittest.TestCase):

    # CSV column set — matches R's format including 'part' column
    _EXPECTED_HEADERS = {
        "name", "scenario", "notes", "topic",
        "question", "part", "response",
        "intellectual_control", "renderability",
    }

    def _check_csv(self, name: str):
        data = _csv_req(name)
        csv_text = build_csv(data)
        self.assertIsInstance(csv_text, str)
        header_line = csv_text.splitlines()[0]
        headers = {h.strip('"') for h in header_line.split(",")}
        self.assertEqual(headers, self._EXPECTED_HEADERS,
                         msg=f"CSV headers wrong for {name!r}")

    def _check_csv_column_order(self, name: str):
        """Column order must match R: name,scenario,notes,topic,question,part,response,ic,renderability."""
        data = _csv_req(name)
        csv_text = build_csv(data)
        header_line = csv_text.splitlines()[0]
        self.assertEqual(
            header_line,
            "name,scenario,notes,topic,question,part,response,intellectual_control,renderability",
        )

    def test_csv_mixed(self):
        self._check_csv("mixed")
        self._check_csv_column_order("mixed")

    def test_csv_mixed_simple(self):
        self._check_csv("mixed_simple")
        self._check_csv_column_order("mixed_simple")

    def test_csv_has_data_rows(self):
        csv_text = build_csv(_csv_req("mixed"))
        lines = [l for l in csv_text.splitlines() if l.strip()]
        self.assertGreater(len(lines), 1)

    def test_csv_expands_multi_part_questions(self):
        """Multi-part questions (e.g. Digital_Object) produce one row per part."""
        csv_text = build_csv(_csv_req("mixed"))
        import csv as csv_mod, io
        reader = csv_mod.DictReader(io.StringIO(csv_text))
        do_rows = [r for r in reader if r["topic"] == "Digital Object"]
        # Digital_Object has 3 parts → 3 rows per model/scenario
        self.assertGreaterEqual(len(do_rows), 3)

    def test_csv_part_column_contains_bullet_text(self):
        """The 'part' column contains the bullet sub-text for multi-part questions."""
        csv_text = build_csv(_csv_req("mixed"))
        import csv as csv_mod, io
        reader = csv_mod.DictReader(io.StringIO(csv_text))
        parts = [r["part"] for r in reader if r["topic"] == "Digital Object" and r["part"]]
        self.assertTrue(any("Born Digital" in p for p in parts))

    def test_lambda_csv_endpoint_returns_200(self):
        event = _lambda_event("/api/report/csv", _csv_req("mixed_simple"))
        resp = handler(event, {})
        self.assertEqual(resp["statusCode"], 200)

    def test_lambda_csv_content_type(self):
        event = _lambda_event("/api/report/csv", _csv_req("mixed_simple"))
        resp = handler(event, {})
        self.assertIn("text/csv", resp["headers"].get("Content-Type", ""))


# ════════════════════════════════════════════════════════════════════════════
# test-plots.R
# ════════════════════════════════════════════════════════════════════════════

class TestPlots(unittest.TestCase):

    _CASES = ["simple_scenario", "simple_scenario2", "simple", "advanced"]

    def test_prepare_chart_data_returns_required_keys(self):
        """_prepare_chart_data() always produces dicts with the required keys."""
        for case in self._CASES:
            with self.subTest(case=case):
                data = _plot_req(case)
                rows = _prepare_chart_data(data)
                self.assertIsInstance(rows, list)
                self.assertGreater(len(rows), 0)
                for row in rows:
                    self.assertIn("model_name", row)
                    self.assertIn("scenario", row)
                    self.assertIn("ic", row)
                    self.assertIn("r", row)

    def test_prepare_chart_data_scores_are_numeric(self):
        for case in self._CASES:
            with self.subTest(case=case):
                rows = _prepare_chart_data(_plot_req(case))
                for row in rows:
                    self.assertIsInstance(row["ic"], (int, float))
                    self.assertIsInstance(row["r"], (int, float))

    def test_render_chart_produces_valid_png(self):
        """render_chart_to_bytes() returns valid PNG bytes for all test cases."""
        for case in self._CASES:
            with self.subTest(case=case):
                png = render_chart_to_bytes(_plot_req(case))
                self.assertIsInstance(png, bytes)
                self.assertEqual(png[:4], bytes([137, 80, 78, 71]),
                                 msg=f"Not a PNG for case {case!r}")

    def test_render_chart_has_sensible_dimensions(self):
        from PIL import Image
        for case in self._CASES:
            with self.subTest(case=case):
                png = render_chart_to_bytes(_plot_req(case))
                with Image.open(io.BytesIO(png)) as img:
                    self.assertGreater(img.size[0], 0)
                    self.assertGreater(img.size[1], 0)

    def test_lambda_chart_endpoint_returns_200(self):
        event = _lambda_event("/api/chart/plot", _plot_req("simple_scenario"))
        resp = handler(event, {})
        self.assertEqual(resp["statusCode"], 200)

    def test_lambda_chart_response_is_base64_png(self):
        event = _lambda_event("/api/chart/plot", _plot_req("simple_scenario"))
        resp = handler(event, {})
        self.assertTrue(resp.get("isBase64Encoded"))
        data = base64.b64decode(resp["body"])
        self.assertEqual(data[:4], bytes([137, 80, 78, 71]))


# ════════════════════════════════════════════════════════════════════════════
# test-pdf.R
# ════════════════════════════════════════════════════════════════════════════

class TestPdf(unittest.TestCase):
    """Mirrors test-pdf.R.

    Note: pdf_requests fixtures contain pre-scored API Gateway payloads where
    ``response`` is null.  The Python generate_pdf() works with extract_responses()
    output (i.e. raw form responses), so score_requests fixtures are used here.
    """

    def _responses(self, name="simple_scenario"):
        return extract_responses(_score_req(name))

    def test_pdf_generation_returns_bytes(self):
        pdf = generate_pdf(self._responses("simple_scenario"))
        self.assertIsInstance(pdf, bytes)

    def test_pdf_starts_with_pdf_header(self):
        pdf = generate_pdf(self._responses("simple_scenario"))
        self.assertEqual(pdf[:4], b"%PDF")

    def test_pdf_simple_scenario(self):
        pdf = generate_pdf(self._responses("simple_scenario"))
        self.assertEqual(pdf[:4], b"%PDF")
        self.assertGreater(len(pdf), 1000)

    def test_pdf_advanced(self):
        pdf = generate_pdf([_advanced_responses()])
        self.assertEqual(pdf[:4], b"%PDF")

    def test_questions_yaml_loads(self):
        """The questions YAML asset can be loaded and has all user nodes."""
        import yaml
        path = os.path.join(
            os.path.dirname(__file__), "assets", "config", "pdf_questions.yml"
        )
        with open(path) as f:
            questions = yaml.safe_load(f)
        for node in USER_NODES:
            self.assertIn(node, questions, msg=f"Node {node!r} missing from questions YAML")

    def test_lambda_pdf_endpoint_returns_200(self):
        event = _lambda_event("/api/report/pdf", _score_req("simple_scenario"))
        resp = handler(event, {})
        self.assertEqual(resp["statusCode"], 200)

    def test_lambda_pdf_content_type(self):
        event = _lambda_event("/api/report/pdf", _score_req("simple_scenario"))
        resp = handler(event, {})
        self.assertEqual(resp["headers"].get("Content-Type"), "application/pdf")

    def test_lambda_pdf_response_is_base64(self):
        event = _lambda_event("/api/report/pdf", _score_req("simple_scenario"))
        resp = handler(event, {})
        self.assertTrue(resp.get("isBase64Encoded"))
        data = base64.b64decode(resp["body"])
        self.assertEqual(data[:4], b"%PDF")


# ════════════════════════════════════════════════════════════════════════════
# test-api.R  (Lambda handler / routing)
# ════════════════════════════════════════════════════════════════════════════

class TestLambdaHandler(unittest.TestCase):

    def test_is_alive(self):
        """GET /api/test/is_alive returns 200 with {"alive": true}."""
        resp = handler({"path": "/api/test/is_alive", "body": None, "isBase64Encoded": False}, {})
        self.assertEqual(resp["statusCode"], 200)
        body = json.loads(resp["body"])
        self.assertTrue(body["alive"])

    def test_score_endpoint_returns_correct_keys(self):
        """Score endpoint for a single item returns a dict with the expected keys."""
        event = _lambda_event("/api/model/score", _score_req("simple_single"))
        resp = handler(event, {})
        self.assertEqual(resp["statusCode"], 200)
        body = json.loads(resp["body"])
        # single-item requests return a dict directly
        self.assertIsInstance(body, dict)
        self.assertIn("intellectual_control", body)
        self.assertIn("renderability", body)
        self.assertIn("nodes", body)

    def test_score_endpoint_scores_in_range(self):
        event = _lambda_event("/api/model/score", _score_req("simple_single"))
        resp = handler(event, {})
        result = json.loads(resp["body"])
        self.assertGreaterEqual(result["intellectual_control"], 0)
        self.assertLessEqual(result["intellectual_control"], 100)
        self.assertGreaterEqual(result["renderability"], 0)
        self.assertLessEqual(result["renderability"], 100)

    def test_score_simple_scenario_model(self):
        """The score endpoint handles a two-entry simple_scenario request."""
        data = _score_req("simple_scenario")
        # The score endpoint only accepts one item at a time; send each individually.
        for item in data:
            event = _lambda_event("/api/model/score", item)
            resp = handler(event, {})
            self.assertEqual(resp["statusCode"], 200)
            result = json.loads(resp["body"])
            self.assertIn("intellectual_control", result)
            self.assertIn("renderability", result)

    def test_unknown_path_returns_404(self):
        resp = handler({"path": "/api/does/not/exist", "body": None, "isBase64Encoded": False}, {})
        self.assertEqual(resp["statusCode"], 404)

    def test_malformed_body_returns_error(self):
        """A non-JSON body to the score endpoint returns a 4xx/5xx status."""
        resp = handler({"path": "/api/model/score", "body": "not json", "isBase64Encoded": False}, {})
        self.assertGreaterEqual(resp["statusCode"], 400)

    def test_validation_endpoint_valid(self):
        event = _lambda_event("/api/validation/validate_json", _csv_req("mixed"))
        resp = handler(event, {})
        self.assertEqual(resp["statusCode"], 200)
        body = json.loads(resp["body"])
        self.assertTrue(body["status"])

    def test_chart_endpoint_returns_png(self):
        event = _lambda_event("/api/chart/plot", _plot_req("simple_scenario"))
        resp = handler(event, {})
        self.assertEqual(resp["statusCode"], 200)
        png = base64.b64decode(resp["body"])
        self.assertEqual(png[:4], bytes([137, 80, 78, 71]))

    def test_csv_endpoint(self):
        event = _lambda_event("/api/report/csv", _csv_req("mixed"))
        resp = handler(event, {})
        self.assertEqual(resp["statusCode"], 200)

    def test_pdf_endpoint(self):
        event = _lambda_event("/api/report/pdf", _score_req("simple_scenario"))
        resp = handler(event, {})
        self.assertEqual(resp["statusCode"], 200)
        pdf = base64.b64decode(resp["body"])
        self.assertEqual(pdf[:4], b"%PDF")


if __name__ == "__main__":
    unittest.main(verbosity=2)
