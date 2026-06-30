# R to Python Migration

This document describes the migration of the DiAGRAM backend API from R to Python, covering the motivation, technical decisions, library replacements, and deployment changes.

## Background

The DiAGRAM backend was originally written as an R package (`diagramLambda`) running on AWS Lambda via a custom R runtime provided by the [{lambdr}](https://github.com/mdneuzerling/lambdr) package. While this worked, it created two significant operational problems:

1. **LaTeX dependency.** PDF report generation used R Markdown with [TinyTeX](https://yihui.org/tinytex/), a minimal LaTeX distribution. Even "minimal" LaTeX is approximately 500 MB, making the container image very large and cold-start times slow.

2. **Non-native Lambda runtime.** R is not a natively supported Lambda runtime. Running it required a custom bootstrap script, a bespoke runtime loop, and special serialisation/deserialisation wiring to handle multiple response types (JSON, PDF, PNG, CSV) from a single Lambda function.

The Python rewrite addresses both problems. Python is a first-class Lambda runtime, and the LaTeX dependency has been eliminated entirely.

## What changed

### Library replacements

| Concern | R package | Python equivalent | Notes |
|---|---|---|---|
| Lambda runtime | `{lambdr}` | Native Python handler | Python is a supported Lambda runtime; no custom bootstrap needed |
| Bayesian network (loading) | `{bnlearn}` / `{gRain}` | Custom `bn_inference.py` | Pure-Python BIF parser and Variable Elimination — no external library |
| Bayesian network (inference) | `{bnlearn}` / `{gRain}` | Custom `bn_inference.py` | Dict-based factor arithmetic; no numpy/scipy required |
| PDF generation | R Markdown + TinyTeX (LaTeX) | `reportlab` | Eliminates the ~500 MB LaTeX dependency |
| Chart generation | `{ggplot2}` | `Pillow` | Image drawing with PIL; already needed by reportlab |
| JSON validation | `{jsonvalidate}` | `jsonschema` (stdlib-compatible) | Standard Python approach |
| HTTP routing | `diagramLambda::handler()` | `lambda_function.handler` | Simple path-based routing in plain Python |

### Dependencies: before and after

The R Lambda required a large Docker image because of TinyTeX. The Python version's dependencies are very small:

| Package | Installed size |
|---|---|
| `reportlab` | ~9 MB |
| `Pillow` | ~24 MB |
| `pyyaml` | ~3 MB |
| **Total** | **~36 MB** |

The previous Python prototype (before the final library rationalisation) also included `pgmpy`, `matplotlib`, `numpy`, `scipy`, `pandas`, and `networkx`, which together added ~385 MB. These were eliminated by writing a purpose-built Bayesian network engine.

### Package size impact

| Version | Unzipped | Compressed | Deployment method |
|---|---|---|---|
| R (Docker image) | ~1+ GB | N/A | Docker image to ECR |
| Python (initial, with pgmpy) | ~246 MiB | ~79 MiB | S3 upload required |
| **Python (final)** | **~36 MiB** | **~8 MiB** | **Direct ZIP upload** |

The final Python version fits within Lambda's 50 MB direct-upload limit. S3 staging is no longer required for deployment.

---

## Architecture

The Python backend follows the same single-function, path-based routing pattern as the R version.

```
API Gateway
    │
    ▼
lambda_function.handler(event, context)
    │
    ├── /api/test/is_alive       → 200 {"alive": true}
    ├── /api/model/score         → model.score_model_()
    ├── /api/chart/plot          → plot.render_chart_to_bytes()
    ├── /api/report/pdf          → pdf_report.generate_pdf()
    ├── /api/report/csv          → csv_report.generate_csv()
    └── /api/validation/validate_json → validate.validate_json()
```

The model is loaded once at module-load time (Lambda cold start) and reused across warm invocations.

### Source files

| File | Purpose |
|---|---|
| `lambda_function.py` | Lambda entry point and request router |
| `bn_inference.py` | Pure-Python Bayesian network: BIF parser, Factor arithmetic, Variable Elimination |
| `model.py` | Load BIF model, apply user CPDs, run inference, return scores |
| `to_numeric.py` | Convert user form responses to numeric values per node |
| `to_probability.py` | Convert numerics to `Factor` objects for CPD replacement |
| `nodes.py` | Node name constants, user-editable node list, state-name map |
| `responses.py` | Parse API request bodies (simple and advanced model formats) |
| `validate.py` | JSON schema validation |
| `plot.py` | Horizontal bar chart generation (Pillow) |
| `pdf_report.py` | PDF generation (reportlab, no LaTeX) |
| `csv_report.py` | CSV report generation |
| `local_server.py` | Thin HTTP wrapper around the Lambda handler for local development |

---

## Key technical decisions

### Pure-Python Bayesian network inference (`bn_inference.py`)

Rather than depending on `pgmpy` (which transitively pulls in PyTorch, scikit-learn, statsmodels, and huggingface_hub — nearly 2 GB when installed naively), a purpose-built engine was written. It covers only what DiAGRAM actually needs:

- **BIF file parser** — tokeniser and recursive-descent parser for the `.bif` format used by the model file.
- **`Factor` class** — a probability table over a set of discrete variables, stored as a plain Python `dict[tuple[int, ...], float]`. No numpy arrays.
- **Variable Elimination** — exact marginal inference using factor multiplication and marginalisation, with a greedy min-degree elimination ordering.

The network has 21 nodes, all binary or ternary. The largest possible intermediate factor during inference has at most a few hundred entries, so pure-Python dict arithmetic is entirely sufficient and adds no dependencies.

### PDF generation without LaTeX (`pdf_report.py`)

The R version used R Markdown to render a parameterised `.Rmd` template to PDF via TinyTeX. This required:
- TinyTeX (~500 MB)
- Workarounds for TinyTeX's hard-coded write paths (only `/tmp` is writable in Lambda)

The Python version uses `reportlab` to construct the PDF programmatically. The same Open Sans fonts and TNA logo that the R version used are bundled in `assets/resources/` and loaded directly by reportlab. There is no LaTeX involved at any stage.

### Chart generation (`plot.py`)

The R version used `{ggplot2}` for horizontal bar charts. The Python version uses `Pillow` (`PIL.ImageDraw`) to draw the same chart layout directly onto a bitmap, then encodes it as PNG. Pillow is already a dependency of reportlab for image embedding, so this adds no extra packages.

---

## Deployment

### Lambda ZIP (recommended)

```bash
cd api/src_python
./build_lambda_zip.sh
```

Produces `dist/diagram_lambda.zip` (~8 MB compressed). Deploy directly:

```bash
aws lambda update-function-code \
    --function-name YOUR_FUNCTION \
    --zip-file fileb://dist/diagram_lambda.zip
```

### Docker image (Lambda container)

The `api/src_python/Dockerfile` uses the official AWS Lambda Python 3.12 base image and is suitable for ECR deployment if a container-based Lambda is preferred.

```bash
docker build -t diagram-api ./api/src_python
```

---

## Local development

A Docker Compose setup runs the full stack locally:

```bash
docker compose up --build
```

| Service | URL | Notes |
|---|---|---|
| Frontend | http://localhost:3000 | Parcel-built static site served by nginx |
| API | http://localhost:8080 | Direct API access (bypasses nginx) |

The `api/src_python/local_server.py` wraps the Lambda handler in a plain HTTP server so the same application code runs locally without the Lambda Runtime Interface Emulator.

nginx proxies `/api/*` requests from the frontend container to the API container, keeping the frontend and API on the same origin and avoiding any CORS configuration.

---

## Testing

Regression values for the Bayesian network scoring (used to verify the Python inference engine produces the same results as the original R implementation):

| Test case | Intellectual Control | Renderability |
|---|---|---|
| Simple model (all defaults) | 0.0 | 44.679 |
| BINGO benchmark model | 38.5835 | 45.4831 |
| Advanced model | 32.0139 | 24.2141 |

To run a quick regression check:

```bash
cd api/src_python
python3 -c "
import sys, json
sys.path.insert(0, '.')
from model import load_default_model, score_model_
from responses import extract_responses

base = load_default_model()

for name, path in [
    ('simple',   '../src/inst/extdata/test_data/score_requests/simple_model.json'),
    ('mixed',    '../src/inst/extdata/test_data/score_requests/mixed_model.json'),
    ('advanced', '../src/inst/extdata/test_data/score_requests/advanced_model.json'),
]:
    with open(path) as f:
        body = json.load(f)
    for i, r in enumerate(extract_responses(body)):
        out = score_model_(r, base)
        print(f'{name}[{i}]: IC={out[\"intellectual_control\"]}, R={out[\"renderability\"]}')
"
```
