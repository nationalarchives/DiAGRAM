"""
PDF report generation using reportlab – no LaTeX/TinyTeX required.

Replicates the content of the R Markdown PDF template (pdf_template.Rmd /
pdf_section.Rmd) using reportlab's Platypus layout engine.
"""

import io
import os
import tempfile
from typing import Optional

from PIL import Image as PILImage

import yaml
from reportlab.lib import colors
from reportlab.lib.enums import TA_LEFT, TA_CENTER
from reportlab.lib.pagesizes import A4
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.units import cm
from reportlab.platypus import (
    BaseDocTemplate, Frame, PageTemplate,
    Paragraph, Spacer, Table, TableStyle,
    Image, KeepTogether, HRFlowable, PageBreak,
)
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont

from nodes import USER_NODE_MAP

_ASSETS_DIR = os.path.join(os.path.dirname(__file__), "assets")
_RESOURCES_DIR = os.path.join(_ASSETS_DIR, "resources")
_CONFIG_DIR = os.path.join(_ASSETS_DIR, "config")

_TNA_LOGO = os.path.join(_RESOURCES_DIR, "tna-logo.png")
_QUESTIONS_FILE = os.path.join(_CONFIG_DIR, "pdf_questions.yml")

_TNA_ORANGE = colors.HexColor("#CF4C00")
_TNA_PURPLE = colors.HexColor("#8400CD")
_TNA_DARK = colors.HexColor("#1A1A1A")

# Register Open Sans fonts if available
_FONTS_REGISTERED = False


def _register_fonts():
    global _FONTS_REGISTERED
    if _FONTS_REGISTERED:
        return
    font_files = {
        "OpenSans": "OpenSans-Regular.ttf",
        "OpenSans-Bold": "OpenSans-Bold.ttf",
        "OpenSans-Italic": "OpenSans-Italic.ttf",
        "OpenSans-BoldItalic": "OpenSans-BoldItalic.ttf",
    }
    try:
        for name, filename in font_files.items():
            path = os.path.join(_RESOURCES_DIR, filename)
            if os.path.exists(path):
                pdfmetrics.registerFont(TTFont(name, path))
        _FONTS_REGISTERED = True
    except Exception:
        pass  # fall back to Helvetica if fonts aren't available


def _get_font(bold=False, italic=False):
    """Return registered font name or Helvetica fallback."""
    if _FONTS_REGISTERED:
        if bold and italic:
            return "OpenSans-BoldItalic"
        if bold:
            return "OpenSans-Bold"
        if italic:
            return "OpenSans-Italic"
        return "OpenSans"
    if bold and italic:
        return "Helvetica-BoldOblique"
    if bold:
        return "Helvetica-Bold"
    if italic:
        return "Helvetica-Oblique"
    return "Helvetica"


def _build_styles():
    _register_fonts()
    styles = getSampleStyleSheet()
    font = _get_font()
    font_bold = _get_font(bold=True)

    base = ParagraphStyle(
        "DiagramBase", parent=styles["Normal"],
        fontName=font, fontSize=10, leading=14, textColor=_TNA_DARK,
    )
    styles.add(base)
    styles.add(ParagraphStyle(
        "DiagramH1", parent=base,
        fontName=font_bold, fontSize=16, leading=20,
        textColor=_TNA_ORANGE, spaceAfter=8,
    ))
    styles.add(ParagraphStyle(
        "DiagramH2", parent=base,
        fontName=font_bold, fontSize=13, leading=18,
        textColor=_TNA_DARK, spaceAfter=6, spaceBefore=10,
    ))
    styles.add(ParagraphStyle(
        "DiagramH3", parent=base,
        fontName=font_bold, fontSize=11, leading=15,
        textColor=_TNA_DARK, spaceAfter=4, spaceBefore=6,
    ))
    styles.add(ParagraphStyle(
        "DiagramBody", parent=base,
        fontName=font, fontSize=9, leading=13, spaceAfter=4,
    ))
    styles.add(ParagraphStyle(
        "DiagramSmall", parent=base,
        fontName=font, fontSize=8, leading=11,
    ))
    styles.add(ParagraphStyle(
        "DiagramBold", parent=base,
        fontName=font_bold, fontSize=9, leading=13,
    ))
    return styles


def _load_questions() -> dict:
    with open(_QUESTIONS_FILE, encoding="utf-8") as fh:
        return yaml.safe_load(fh)


def _flatten_question(q) -> str:
    """Convert question value (string, list, or dict) to a plain string."""
    if isinstance(q, list):
        return "\n".join(str(v) for v in q)
    return str(q).strip()


def _flatten_response(resp) -> str:
    """Convert a response value to a display string."""
    if isinstance(resp, list):
        return "\n\n".join(f"- {v}" for v in resp)
    if isinstance(resp, dict):
        return "\n".join(f"{v}" for v in resp.values())
    return str(resp)


def _expand_question_rows(node_key: str, question_raw, response_raw) -> list[tuple[str, str]]:
    """Expand a question/response into (question_text, response_text) pairs.

    Mirrors R's pdf_table_part():
    - Dict questions (e.g. Rep_and_Refresh, System_Security): one row per sub-question.
    - List sub-questions (e.g. Info_Management.3): one row per list item.
    - Multi-answer array questions (e.g. Digital_Object): format responses as bullet list.
    - Simple scalar: one row.
    """
    if isinstance(question_raw, dict):
        rows = []
        for key in sorted(question_raw.keys(), key=lambda k: int(k) if str(k).isdigit() else k):
            sub_q = question_raw[key]
            sub_r = response_raw.get(str(key)) if isinstance(response_raw, dict) else None
            if isinstance(sub_q, list):
                sub_r_list = sub_r if isinstance(sub_r, list) else [sub_r] * len(sub_q)
                for sq, sr in zip(sub_q, sub_r_list):
                    rows.append((sq.strip(), str(sr)))
            else:
                rows.append((sub_q.strip(), str(sub_r) if sub_r is not None else ""))
        return rows

    # Scalar question — if response has more items, format as bullet list
    q_text = _flatten_question(question_raw)
    if isinstance(response_raw, list):
        r_text = "\n\n".join(f"- {v}" for v in response_raw)
    else:
        r_text = _flatten_response(response_raw)
    return [(q_text, r_text)]


def _table_style(has_header=True):
    cmds = [
        ("FONTNAME", (0, 0), (-1, -1), _get_font()),
        ("FONTSIZE", (0, 0), (-1, -1), 8),
        ("TOPPADDING", (0, 0), (-1, -1), 4),
        ("BOTTOMPADDING", (0, 0), (-1, -1), 4),
        ("LEFTPADDING", (0, 0), (-1, -1), 6),
        ("RIGHTPADDING", (0, 0), (-1, -1), 6),
        ("ROWBACKGROUNDS", (0, 0), (-1, -1), [colors.white, colors.HexColor("#F5F5F5")]),
        ("BOX", (0, 0), (-1, -1), 0.5, colors.HexColor("#CCCCCC")),
        ("INNERGRID", (0, 0), (-1, -1), 0.25, colors.HexColor("#DDDDDD")),
        ("VALIGN", (0, 0), (-1, -1), "TOP"),
        ("WORDWRAP", (0, 0), (-1, -1), True),
    ]
    if has_header:
        cmds += [
            ("BACKGROUND", (0, 0), (-1, 0), _TNA_ORANGE),
            ("TEXTCOLOR", (0, 0), (-1, 0), colors.white),
            ("FONTNAME", (0, 0), (-1, 0), _get_font(bold=True)),
            ("FONTSIZE", (0, 0), (-1, 0), 9),
        ]
    return TableStyle(cmds)


def _build_responses_table(resp: dict, questions: dict, styles) -> list:
    """Build a reportlab table for the baseline model responses.

    resp is a full response object from extract_responses().
    Dict questions (e.g. Rep_and_Refresh) are expanded to one row per sub-question,
    mirroring R's pdf_table_part() behaviour.
    """
    response = resp.get("data", {}).get("response", {})
    col_widths = [3.5 * cm, 8 * cm, 5.5 * cm]
    data = [["Topic", "Question", "Response"]]

    for node_key in USER_NODE_MAP:
        topic = USER_NODE_MAP[node_key]
        question_raw = questions.get(node_key)
        response_raw = response.get(node_key)

        if question_raw is None or response_raw is None:
            continue

        for q_text, r_text in _expand_question_rows(node_key, question_raw, response_raw):
            data.append([
                Paragraph(topic, styles["DiagramSmall"]),
                Paragraph(q_text.replace("\n", "<br/>"), styles["DiagramSmall"]),
                Paragraph(r_text.replace("\n", "<br/>"), styles["DiagramSmall"]),
            ])

    if len(data) == 1:
        return []

    t = Table(data, colWidths=col_widths, repeatRows=1)
    t.setStyle(_table_style())
    return [t]


def _build_diff_table(scenario_resp: dict, base_resp: dict, questions: dict, styles) -> list:
    """Build a diff table showing how a scenario differs from the base model.

    Both arguments are full response objects from extract_responses().
    """
    base_response = base_resp.get("data", {}).get("response", {})
    scen_response = scenario_resp.get("data", {}).get("response", {})

    diffs = []
    for node_key in USER_NODE_MAP:
        base_val = base_response.get(node_key)
        scen_val = scen_response.get(node_key)
        if base_val is None or scen_val is None:
            continue
        if _flatten_response(base_val) != _flatten_response(scen_val):
            topic = USER_NODE_MAP[node_key]
            q = questions.get(node_key)
            # For diff table, use expanded rows so dict questions show legibly
            base_rows = _expand_question_rows(node_key, q, base_val) if q else [("", _flatten_response(base_val))]
            scen_rows = _expand_question_rows(node_key, q, scen_val) if q else [("", _flatten_response(scen_val))]
            # Zip them up — both lists have the same length for the same node
            for (q_text, b_text), (_, s_text) in zip(base_rows, scen_rows):
                if b_text != s_text:
                    diffs.append({
                        "topic": topic,
                        "question": q_text,
                        "scenario_response": s_text,
                        "base_response": b_text,
                    })

    if not diffs:
        return [Paragraph("No differences from the base model.", styles["DiagramBody"])]

    col_widths = [2.5 * cm, 5 * cm, 4 * cm, 4 * cm]
    data = [["Topic", "Question", "Scenario Response", "Base Model Response"]]
    for d in diffs:
        data.append([
            Paragraph(d["topic"], styles["DiagramSmall"]),
            Paragraph(d["question"].replace("\n", "<br/>"), styles["DiagramSmall"]),
            Paragraph(d["scenario_response"].replace("\n", "<br/>"), styles["DiagramSmall"]),
            Paragraph(d["base_response"].replace("\n", "<br/>"), styles["DiagramSmall"]),
        ])

    t = Table(data, colWidths=col_widths, repeatRows=1)
    t.setStyle(_table_style())
    return [t]


def _on_page(canvas, doc, styles, logo_path):
    """Draw header/footer on each page."""
    canvas.saveState()
    w, h = A4
    # Footer
    canvas.setFont(_get_font(), 8)
    canvas.setFillColor(colors.HexColor("#888888"))
    canvas.drawString(2 * cm, 1.2 * cm, "DiAGRAM – Digital Archiving Graphical Risk Assessment Model")
    canvas.drawRightString(w - 2 * cm, 1.2 * cm, f"Page {doc.page}")
    # Logo in top-right
    if os.path.exists(logo_path):
        try:
            canvas.drawImage(logo_path, w - 4.5 * cm, h - 2.2 * cm,
                             width=3.5 * cm, height=1.2 * cm, preserveAspectRatio=True)
        except Exception:
            pass
    canvas.restoreState()


def _group_by_model(responses: list) -> dict:
    """Group response objects by model_name."""
    groups = {}
    for resp in responses:
        name = resp.get("model_name") or "Unknown"
        groups.setdefault(name, []).append(resp)
    return groups


def generate_pdf(responses: list) -> bytes:
    """Generate a PDF report for the given list of response objects.

    Returns raw PDF bytes.
    """
    from plot import render_chart_to_bytes  # avoid circular import at module level

    _register_fonts()
    styles = _build_styles()
    questions = _load_questions()

    buf = io.BytesIO()
    doc = BaseDocTemplate(
        buf, pagesize=A4,
        leftMargin=2 * cm, rightMargin=2 * cm,
        topMargin=3 * cm, bottomMargin=2.5 * cm,
        title="DiAGRAM Report",
    )

    frame = Frame(
        doc.leftMargin, doc.bottomMargin,
        doc.width, doc.height, id="main",
    )

    def make_page(canvas, doc_ref):
        _on_page(canvas, doc_ref, styles, _TNA_LOGO)

    doc.addPageTemplates([PageTemplate(id="main", frames=[frame], onPage=make_page)])

    story = []

    # ── Intro ──────────────────────────────────────────────────────────────────
    story.append(Spacer(1, 0.5 * cm))
    story.append(Paragraph("DiAGRAM Report", styles["DiagramH1"]))
    story.append(Spacer(1, 0.3 * cm))
    intro = (
        "DiAGRAM produces a risk prevention score for the digital preservation "
        "outcomes of renderability and intellectual control."
    )
    story.append(Paragraph(intro, styles["DiagramBody"]))
    story.append(Spacer(1, 0.2 * cm))
    story.append(Paragraph(
        "&#x2022; <b>Renderability</b> – The object is a sufficiently useful "
        "representation of the original file.", styles["DiagramBody"],
    ))
    story.append(Paragraph(
        "&#x2022; <b>Intellectual Control</b> – Having full knowledge of the "
        "material content, provenance and conditions of use.", styles["DiagramBody"],
    ))
    story.append(Spacer(1, 0.2 * cm))
    story.append(Paragraph(
        "The higher the score, the lower the risk to the digital archive.",
        styles["DiagramBody"],
    ))
    story.append(Spacer(1, 0.5 * cm))

    # ── One section per model ──────────────────────────────────────────────────
    model_groups = _group_by_model(responses)

    for model_idx, (model_name, model_responses) in enumerate(model_groups.items()):
        if model_idx > 0:
            story.append(PageBreak())

        story.append(Paragraph(f"Model: {model_name}", styles["DiagramH1"]))
        story.append(HRFlowable(width="100%", thickness=1, color=_TNA_ORANGE))
        story.append(Spacer(1, 0.3 * cm))

        # Bar chart
        try:
            chart_bytes = render_chart_to_bytes(model_responses)
            with PILImage.open(io.BytesIO(chart_bytes)) as _img:
                img_w, img_h = _img.size
            aspect = img_h / img_w
            chart_img = Image(io.BytesIO(chart_bytes))
            chart_img.drawWidth = doc.width
            chart_img.drawHeight = doc.width * aspect
            story.append(chart_img)
        except Exception as exc:
            story.append(Paragraph(f"[Chart unavailable: {exc}]", styles["DiagramSmall"]))
        story.append(Spacer(1, 0.4 * cm))

        # Comments section
        story.append(Paragraph("Comments", styles["DiagramH2"]))
        has_comments = any(r.get("notes", "").strip() for r in model_responses)
        if not has_comments:
            story.append(Paragraph(
                "There were no comments attached to this model.", styles["DiagramBody"],
            ))
        else:
            for resp in model_responses:
                note = resp.get("notes", "").strip()
                scenario = resp.get("scenario", "")
                if note:
                    story.append(Paragraph(
                        f"<b>{scenario}:</b> {note}", styles["DiagramBody"],
                    ))
        story.append(Spacer(1, 0.4 * cm))

        story.append(Paragraph("Responses to Questions", styles["DiagramH2"]))

        # Find the base model (scenario == "Base Model" or first in list)
        base = next((r for r in model_responses if r.get("scenario") == "Base Model"),
                    model_responses[0])
        policies = [r for r in model_responses if r is not base]

        any_policies = len(policies) > 0
        if any_policies:
            story.append(Paragraph(
                "Here is a summary of the responses given for the model and each scenario.",
                styles["DiagramBody"],
            ))
        else:
            story.append(Paragraph(
                "Here is a summary of the responses given for the model.",
                styles["DiagramBody"],
            ))
        story.append(Spacer(1, 0.3 * cm))

        # Baseline responses table
        story.append(Paragraph("Baseline Model", styles["DiagramH3"]))
        story.extend(_build_responses_table(base, questions, styles))
        story.append(Spacer(1, 0.4 * cm))

        # Scenario diff tables
        for policy in policies:
            scen_name = policy.get("scenario", "Scenario")
            story.append(KeepTogether([
                Paragraph(scen_name, styles["DiagramH3"]),
                Paragraph(
                    f"The following responses were changed for {scen_name}.",
                    styles["DiagramBody"],
                ),
            ]))
            story.append(Spacer(1, 0.2 * cm))
            story.extend(_build_diff_table(policy, base, questions, styles))
            story.append(Spacer(1, 0.4 * cm))

    doc.build(story)
    buf.seek(0)
    return buf.read()


def write_temp_pdf(responses: list) -> str:
    """Generate the PDF and write to a temporary file; return the file path."""
    pdf_bytes = generate_pdf(responses)
    tmp = tempfile.NamedTemporaryFile(suffix=".pdf", delete=False)
    tmp.write(pdf_bytes)
    tmp.close()
    return tmp.name
