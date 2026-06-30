"""
Bar-chart generation using Pillow — replaces matplotlib to minimise package size.
"""

import io
import os
import tempfile

from PIL import Image, ImageDraw, ImageFont

_COLOUR_IC = (255, 110, 58)    # #FF6E3A
_COLOUR_R = (132, 0, 205)      # #8400CD
_COLOUR_BG = (255, 255, 255)
_COLOUR_TEXT = (40, 40, 40)
_COLOUR_AXIS = (180, 180, 180)

_FONT_PATH = os.path.join(
    os.path.dirname(__file__), "assets", "resources", "OpenSans-Regular.ttf"
)
_FONT_BOLD_PATH = os.path.join(
    os.path.dirname(__file__), "assets", "resources", "OpenSans-Bold.ttf"
)


def _load_font(path: str, size: int) -> ImageFont.FreeTypeFont:
    try:
        return ImageFont.truetype(path, size)
    except Exception:
        return ImageFont.load_default()


def _prepare_chart_data(data: list) -> list:
    seen = set()
    rows = []
    for item in data:
        key = (item.get("model_name"), item.get("scenario"),
               item.get("intellectual_control"), item.get("renderability"))
        if key in seen:
            continue
        seen.add(key)
        rows.append({
            "model_name": item.get("model_name", ""),
            "scenario": item.get("scenario", ""),
            "ic": item.get("intellectual_control") or 0,
            "r": item.get("renderability") or 0,
        })
    return rows


def _render_chart(data: list) -> Image.Image:
    rows = _prepare_chart_data(data)

    # Group by model
    models = list(dict.fromkeys(r["model_name"] for r in rows))

    # Layout constants (pixels, at 150 DPI equivalent)
    MARGIN_LEFT = 220
    MARGIN_RIGHT = 80
    MARGIN_TOP = 50
    MARGIN_BOTTOM = 70
    BAR_HEIGHT = 18
    BAR_GAP = 6        # gap between IC and R bars
    SCENARIO_HEIGHT = BAR_HEIGHT * 2 + BAR_GAP + 20  # height per scenario
    FACET_GAP = 40
    FACET_TITLE_H = 30
    WIDTH = 900
    MAX_SCORE = 100
    BAR_AREA = WIDTH - MARGIN_LEFT - MARGIN_RIGHT

    font = _load_font(_FONT_PATH, 16)
    font_small = _load_font(_FONT_PATH, 13)
    font_bold = _load_font(_FONT_BOLD_PATH, 17)
    font_legend = _load_font(_FONT_PATH, 15)

    # Pre-calculate total height
    total_height = MARGIN_TOP + MARGIN_BOTTOM
    model_scenarios = {}
    for m in models:
        scens = list(dict.fromkeys(r["scenario"] for r in rows if r["model_name"] == m))
        model_scenarios[m] = scens
        total_height += FACET_TITLE_H + len(scens) * SCENARIO_HEIGHT + FACET_GAP

    img = Image.new("RGB", (WIDTH, total_height), _COLOUR_BG)
    draw = ImageDraw.Draw(img)

    y = MARGIN_TOP

    for model_name in models:
        scens = model_scenarios[model_name]

        # Facet title
        draw.text((MARGIN_LEFT, y), model_name, font=font_bold, fill=_COLOUR_TEXT)
        y += FACET_TITLE_H

        # Axis line
        axis_y = y + len(scens) * SCENARIO_HEIGHT
        draw.line([(MARGIN_LEFT, y), (MARGIN_LEFT, axis_y)], fill=_COLOUR_AXIS, width=1)
        draw.line([(MARGIN_LEFT, axis_y), (WIDTH - MARGIN_RIGHT, axis_y)], fill=_COLOUR_AXIS, width=1)

        # Axis tick labels
        for pct in range(0, 101, 25):
            x = MARGIN_LEFT + int(pct / MAX_SCORE * BAR_AREA)
            draw.line([(x, axis_y), (x, axis_y + 4)], fill=_COLOUR_AXIS, width=1)
            label = str(pct)
            bbox = draw.textbbox((0, 0), label, font=font_small)
            lw = bbox[2] - bbox[0]
            draw.text((x - lw // 2, axis_y + 6), label, font=font_small, fill=_COLOUR_TEXT)

        for si, scenario in enumerate(scens):
            row = next(r for r in rows if r["model_name"] == model_name and r["scenario"] == scenario)
            bar_top = y + si * SCENARIO_HEIGHT + 4

            # Scenario label (right-aligned in the left margin)
            label = scenario if len(scenario) <= 28 else scenario[:25] + "…"
            bbox = draw.textbbox((0, 0), label, font=font)
            lw = bbox[2] - bbox[0]
            lh = bbox[3] - bbox[1]
            label_y = bar_top + (BAR_HEIGHT * 2 + BAR_GAP) // 2 - lh // 2
            draw.text((MARGIN_LEFT - lw - 8, label_y), label, font=font, fill=_COLOUR_TEXT)

            # IC bar
            ic_val = row["ic"]
            ic_w = max(2, int(ic_val / MAX_SCORE * BAR_AREA))
            draw.rectangle(
                [MARGIN_LEFT, bar_top, MARGIN_LEFT + ic_w, bar_top + BAR_HEIGHT],
                fill=_COLOUR_IC,
            )
            draw.text((MARGIN_LEFT + ic_w + 4, bar_top), f"{ic_val}", font=font_small, fill=_COLOUR_IC)

            # R bar
            r_val = row["r"]
            r_w = max(2, int(r_val / MAX_SCORE * BAR_AREA))
            r_top = bar_top + BAR_HEIGHT + BAR_GAP
            draw.rectangle(
                [MARGIN_LEFT, r_top, MARGIN_LEFT + r_w, r_top + BAR_HEIGHT],
                fill=_COLOUR_R,
            )
            draw.text((MARGIN_LEFT + r_w + 4, r_top), f"{r_val}", font=font_small, fill=_COLOUR_R)

        y += len(scens) * SCENARIO_HEIGHT + FACET_GAP

    # Legend at the bottom
    legend_y = total_height - MARGIN_BOTTOM + 10
    swatch_size = 14
    draw.rectangle([MARGIN_LEFT, legend_y, MARGIN_LEFT + swatch_size, legend_y + swatch_size], fill=_COLOUR_IC)
    draw.text((MARGIN_LEFT + swatch_size + 6, legend_y), "Intellectual Control", font=font_legend, fill=_COLOUR_TEXT)
    ic_bbox = draw.textbbox((0, 0), "Intellectual Control", font=font_legend)
    ic_label_w = ic_bbox[2] - ic_bbox[0]
    r_legend_x = MARGIN_LEFT + swatch_size + 6 + ic_label_w + 30
    draw.rectangle([r_legend_x, legend_y, r_legend_x + swatch_size, legend_y + swatch_size], fill=_COLOUR_R)
    draw.text((r_legend_x + swatch_size + 6, legend_y), "Renderability", font=font_legend, fill=_COLOUR_TEXT)

    return img


def write_temp_png(data: list) -> str:
    """Render bar chart to a temporary PNG file and return the path."""
    img = _render_chart(data)
    tmp = tempfile.NamedTemporaryFile(suffix=".png", delete=False)
    img.save(tmp.name, format="PNG")
    tmp.close()
    return tmp.name


def render_chart_to_bytes(data: list) -> bytes:
    """Render bar chart and return PNG bytes (used by pdf_report)."""
    img = _render_chart(data)
    buf = io.BytesIO()
    img.save(buf, format="PNG")
    buf.seek(0)
    return buf.read()
