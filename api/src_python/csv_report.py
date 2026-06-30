"""
CSV report generation.

Row expansion mirrors R's csv_part() function:
- Dict questions (Op_Environment, System_Security, etc.): one row per sub-question.
- Multi-part array questions (Digital_Object, Technical_Skills, etc.): one row per bullet
  point, obtained by splitting the question text on '\\n\\n- '.
- List sub-questions (Info_Management.3): one row per list item.
- Simple scalar questions: one row.
Column order matches R: name, scenario, notes, topic, question, part, response,
intellectual_control, renderability.
"""

import csv
import io
import os

import yaml

from nodes import USER_NODE_MAP

_QUESTIONS_FILE = os.path.join(
    os.path.dirname(__file__), "assets", "config", "pdf_questions.yml"
)

_questions_cache: dict | None = None


def _load_questions() -> dict:
    global _questions_cache
    if _questions_cache is None:
        with open(_QUESTIONS_FILE, encoding="utf-8") as fh:
            _questions_cache = yaml.safe_load(fh)
    return _questions_cache


def _csv_part(question, response, topic: str) -> list[dict]:
    """Expand a single node's question/response into one or more row dicts.

    Mirrors R's csv_part() function logic:
    - is_complex (dict question): map over key-value pairs, one row per sub-question.
    - multi-part string (contains '\\n\\n- '): split into intro + parts, one row per part.
    - simple: one row.
    """
    rows = []

    if isinstance(question, dict):
        # Complex multi-part: iterate sub-questions in key order
        for key in sorted(question.keys(), key=lambda k: int(k) if str(k).isdigit() else k):
            sub_q = question[key]
            sub_r = response.get(str(key)) if isinstance(response, dict) else None

            if isinstance(sub_q, list):
                # Sub-question is itself a list (e.g. Info_Management.3)
                sub_r_list = sub_r if isinstance(sub_r, list) else [sub_r] * len(sub_q)
                for sq, sr in zip(sub_q, sub_r_list):
                    rows.append({"topic": topic, "question": sq, "part": None, "response": sr})
            else:
                rows.append({"topic": topic, "question": sub_q, "part": None, "response": sub_r})

    elif isinstance(question, str) and "\n\n- " in question:
        # Multi-part array question: split on separator, pair with responses
        parts = question.split("\n\n- ")
        intro = parts[0]
        part_texts = parts[1:]
        r_list = response if isinstance(response, list) else [response]
        for part_text, resp_val in zip(part_texts, r_list):
            rows.append({"topic": topic, "question": intro, "part": part_text, "response": resp_val})

    else:
        # Simple single-row question
        rows.append({"topic": topic, "question": question, "part": None, "response": response})

    return rows


def _build_rows_for_model(model_resp: dict, questions: dict) -> list:
    """Build a list of row dicts for a single model/scenario."""
    response = model_resp.get("data", {}).get("response", {})
    name = model_resp.get("model_name", "")
    scenario = model_resp.get("scenario", "")
    notes = model_resp.get("notes", "")
    ic = model_resp.get("intellectual_control", "")
    renderability = model_resp.get("renderability", "")

    rows = []
    for node_key in USER_NODE_MAP:
        topic = USER_NODE_MAP[node_key]
        q = questions.get(node_key)
        r = response.get(node_key)
        if q is None or r is None:
            continue
        for part_row in _csv_part(q, r, topic):
            rows.append({
                "name": name,
                "scenario": scenario,
                "notes": notes,
                "topic": part_row["topic"],
                "question": part_row["question"],
                "part": part_row["part"],
                "response": part_row["response"],
                "intellectual_control": ic,
                "renderability": renderability,
            })
    return rows


def build_csv(parsed_json: list | dict) -> str:
    """Build a CSV string from a list of parsed response objects.

    Only includes non-advanced (simple) models, mirroring the R implementation.
    Column order: name, scenario, notes, topic, question, part, response,
    intellectual_control, renderability.
    """
    from responses import extract_responses

    if isinstance(parsed_json, dict):
        parsed_json = [parsed_json]

    all_responses = extract_responses(parsed_json)
    questions = _load_questions()

    fieldnames = [
        "name", "scenario", "notes",
        "topic", "question", "part", "response",
        "intellectual_control", "renderability",
    ]

    buf = io.StringIO()
    writer = csv.DictWriter(buf, fieldnames=fieldnames, lineterminator="\n",
                            extrasaction="ignore")
    writer.writeheader()

    for resp in all_responses:
        if resp.get("type") == "advanced_responses":
            continue
        rows = _build_rows_for_model(resp, questions)
        for row in rows:
            writer.writerow(row)

    return buf.getvalue()
