# DiAGRAM Python API

Python rewrite of the R Lambda backend for the DiAGRAM (Digital Archiving
Graphical Risk Assessment Model) application.

## Key improvements over the R version

| Concern | R version | Python version |
|---|---|---|
| PDF generation | LaTeX / TinyTeX (≈500 MB) | reportlab (pure Python, ≈10 MB) |
| Runtime | Custom R Lambda bootstrap | Standard AWS Python Lambda base image |
| Cold-start | Slow (R + package load) | Faster (Python + pgmpy) |
| Package size | Exceeds standard Lambda limit; requires Docker | Fits in a standard Docker Lambda |

## Routes

| Method | Path | Description |
|---|---|---|
| POST | `/api/test/is_alive` | Health check |
| POST | `/api/model/score` | Bayesian-network scoring |
| POST | `/api/chart/plot` | Bar-chart PNG |
| POST | `/api/report/pdf` | PDF report (no LaTeX required) |
| POST | `/api/report/csv` | CSV report |
| POST | `/api/validation/validate_json` | JSON validation |

## Running locally with Docker

```bash
# Build the image
docker build -t diagram-api ./api/src_python

# Start the Lambda Runtime Interface Emulator (bundled in the base image)
docker run -p 9000:8080 diagram-api

# Test endpoints
curl -s -XPOST "http://localhost:9000/2015-03-31/functions/function/invocations" \
  -H "Content-Type: application/json" \
  -d '{"path":"/api/test/is_alive","body":null,"isBase64Encoded":false}' \
  | python3 -m json.tool

# Score a model
curl -s -XPOST "http://localhost:9000/2015-03-31/functions/function/invocations" \
  -H "Content-Type: application/json" \
  -d "{
    \"path\": \"/api/model/score\",
    \"isBase64Encoded\": false,
    \"body\": $(cat api/src/inst/extdata/test_data/score_requests/simple_model.json | python3 -c 'import sys,json; print(json.dumps(sys.stdin.read()))')
  }" | python3 -c 'import sys,json; r=json.load(sys.stdin); body=json.loads(r["body"]); print("IC:", body["intellectual_control"], " R:", body["renderability"])'
```

## Lambda event format

The handler expects API Gateway v1 (REST API) proxy events:

```json
{
  "path": "/api/model/score",
  "body": "<JSON string>",
  "isBase64Encoded": false
}
```

Binary responses (PNG / PDF) are returned with `"isBase64Encoded": true` and a
base64-encoded body, which API Gateway will decode automatically.

## Dependencies

All dependencies are pure Python or have pre-compiled wheels — **no LaTeX,
no system R, no pandoc required**.

- **pgmpy** – Bayesian network inference (replaces bnlearn + gRain)
- **matplotlib** – chart generation (replaces ggplot2)
- **reportlab** – PDF generation (replaces rmarkdown + LaTeX)
- **Pillow** – image processing
- **PyYAML** – configuration files
- **pandas / numpy / scipy / networkx** – supporting libraries for pgmpy

## Project structure

```
src_python/
├── lambda_function.py   # AWS Lambda handler & routing
├── model.py             # Bayesian-network loading, CPD modification, inference
├── nodes.py             # Node name constants
├── to_numeric.py        # User responses → numeric values
├── to_probability.py    # Numeric values → pgmpy TabularCPDs
├── responses.py         # JSON request body parsing
├── validate.py          # JSON schema validation
├── plot.py              # Bar-chart generation (matplotlib)
├── pdf_report.py        # PDF report generation (reportlab)
├── csv_report.py        # CSV report generation
├── requirements.txt
├── Dockerfile
└── assets/
    ├── model/model.bif          # Bayesian network definition
    ├── config/pdf_questions.yml # Question text for PDF/CSV reports
    └── resources/               # Fonts and TNA logo
```
