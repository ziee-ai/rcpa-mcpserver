#!/usr/bin/env bash
# Minimal initialize -> tools/list smoke test against a running server.
set -euo pipefail

PORT="${RCPA_PORT:-9004}"
URL="http://localhost:${PORT}/mcp"

INIT_BODY='{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","clientInfo":{"name":"smoke","version":"0"},"capabilities":{}}}'

echo "--- initialize ---"
SID=$(curl -sS -i -X POST "$URL" \
  -H 'Content-Type: application/json' \
  -H 'Accept: application/json, text/event-stream' \
  -H 'Origin: http://localhost' \
  -d "$INIT_BODY" \
  | tee /dev/stderr \
  | awk -v IGNORECASE=1 '/^Mcp-Session-Id:/ { print $2; exit }' \
  | tr -d '\r\n')

echo "Session id: $SID"
[ -n "$SID" ] || { echo "no session id"; exit 1; }

echo
echo "--- notifications/initialized ---"
curl -sS -X POST "$URL" \
  -H 'Content-Type: application/json' \
  -H 'Accept: application/json, text/event-stream' \
  -H "Mcp-Session-Id: $SID" \
  -H 'Origin: http://localhost' \
  -d '{"jsonrpc":"2.0","method":"notifications/initialized"}'

echo
echo "--- tools/list ---"
curl -sS -X POST "$URL" \
  -H 'Content-Type: application/json' \
  -H 'Accept: application/json, text/event-stream' \
  -H "Mcp-Session-Id: $SID" \
  -H 'Origin: http://localhost' \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/list"}' | jq '.result.tools[] | .name'
