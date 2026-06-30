#!/usr/bin/env bash
# build_lambda_zip.sh – Package the DiAGRAM Python Lambda for deployment.
#
# Dependencies are reportlab + Pillow + pyyaml only (~24 MB unzipped, ~8 MB
# compressed), so the zip fits within Lambda's 50 MB direct-upload limit.
#
# Run from any directory — the script resolves paths relative to the repo root.
#
# Usage:
#   .github/scripts/build_lambda_zip.sh [--arch x86_64|arm64] [--python 3.11|3.12] [--output-dir PATH]
#
# Deployment:
#   aws lambda update-function-code \
#       --function-name YOUR_FUNCTION \
#       --zip-file fileb://dist/diagram_lambda.zip

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT_DIR="$REPO_ROOT/api/src_python"
ARCH="x86_64"
PYTHON_VERSION="3.12"
OUTPUT_DIR="$REPO_ROOT/dist"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --arch)        ARCH="$2";           shift 2 ;;
    --python)      PYTHON_VERSION="$2"; shift 2 ;;
    --output-dir)  OUTPUT_DIR="$2";     shift 2 ;;
    *) echo "Unknown option: $1"; exit 1 ;;
  esac
done

case "$ARCH" in
  x86_64) PLATFORM="manylinux2014_x86_64"  ;;
  arm64)  PLATFORM="manylinux2014_aarch64" ;;
  *) echo "Unknown arch: $ARCH (use x86_64 or arm64)"; exit 1 ;;
esac

BUILD_DIR="$(mktemp -d)"
trap 'rm -rf "$BUILD_DIR"' EXIT

echo "DiAGRAM Lambda ZIP builder"
echo "=========================="
echo "  Architecture : $ARCH"
echo "  Python       : $PYTHON_VERSION"
echo "  Output dir   : $OUTPUT_DIR"

# 1. Install dependencies
echo ""
echo "[1/3] Installing dependencies (arch=$ARCH, python=$PYTHON_VERSION) ..."
pip install \
  --platform "$PLATFORM" \
  --implementation cp \
  --python-version "$PYTHON_VERSION" \
  --only-binary :all: \
  --target "$BUILD_DIR" \
  --no-cache-dir --quiet \
  -r "$SCRIPT_DIR/requirements.txt" \
|| {
  echo "  ⚠  Cross-platform install failed; falling back to host platform."
  echo "     (Zip may not run on Lambda if built on a non-Linux host.)"
  pip install \
    --target "$BUILD_DIR" \
    --no-cache-dir --quiet \
    -r "$SCRIPT_DIR/requirements.txt"
}

# 2. Copy application source and assets
echo "[2/3] Copying application source ..."
for f in lambda_function.py model.py bn_inference.py nodes.py to_numeric.py \
          to_probability.py responses.py validate.py plot.py pdf_report.py csv_report.py; do
  [[ -f "$SCRIPT_DIR/$f" ]] && cp "$SCRIPT_DIR/$f" "$BUILD_DIR/" || echo "  ⚠  Not found: $f"
done
cp -r "$SCRIPT_DIR/assets" "$BUILD_DIR/assets"

# 3. Zip everything
echo "[3/3] Building zip ..."
mkdir -p "$OUTPUT_DIR"
ZIP_PATH="$OUTPUT_DIR/diagram_lambda.zip"
(cd "$BUILD_DIR" && zip -qr "$ZIP_PATH" .)

UNZIPPED_KB=$(du -sk "$BUILD_DIR" | cut -f1)
ZIPPED_KB=$(du -sk "$ZIP_PATH" | cut -f1)
echo ""
echo "  ✓  diagram_lambda.zip"
echo "     Unzipped : ~${UNZIPPED_KB} KiB"
echo "     Zipped   : ~${ZIPPED_KB} KiB"
echo ""
echo "Deploy with:"
echo "  aws lambda update-function-code \\"
echo "      --function-name YOUR_FUNCTION \\"
echo "      --zip-file fileb://$ZIP_PATH"
