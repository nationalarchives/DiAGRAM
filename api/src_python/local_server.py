"""
Local HTTP server that wraps the Lambda handler for Docker Compose development.

Converts plain HTTP requests into Lambda-style events so the same
lambda_function.handler code runs locally without the Lambda RIE.
"""

import base64
import os
import sys
from http.server import BaseHTTPRequestHandler, HTTPServer

sys.path.insert(0, os.path.dirname(__file__))
from lambda_function import handler


class LambdaHandler(BaseHTTPRequestHandler):
    def log_message(self, fmt, *args):
        print(f"[API] {self.address_string()} - {fmt % args}", flush=True)

    def do_GET(self):
        self._handle(body=None)

    def do_POST(self):
        length = int(self.headers.get("Content-Length", 0))
        body = self.rfile.read(length).decode("utf-8") if length else None
        self._handle(body=body)

    def _handle(self, body):
        event = {
            "path": self.path.split("?")[0],
            "body": body,
            "isBase64Encoded": False,
        }
        result = handler(event, {})

        status = result.get("statusCode", 200)
        headers = result.get("headers", {})
        response_body = result.get("body", "")
        is_b64 = result.get("isBase64Encoded", False)

        self.send_response(status)
        for key, value in headers.items():
            self.send_header(key, value)
        self.end_headers()

        if is_b64:
            self.wfile.write(base64.b64decode(response_body))
        elif isinstance(response_body, str):
            self.wfile.write(response_body.encode("utf-8"))
        else:
            self.wfile.write(response_body)


if __name__ == "__main__":
    port = int(os.environ.get("PORT", 8080))
    print(f"[API] Starting local server on port {port}", flush=True)
    HTTPServer(("0.0.0.0", port), LambdaHandler).serve_forever()
