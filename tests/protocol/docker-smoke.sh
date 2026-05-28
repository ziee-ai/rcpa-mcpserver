#!/usr/bin/env bash
# Smoke-test the Docker container:
#  1. Start the container, wait for the MCP server to come up
#  2. POST initialize -> assert serverInfo.name = rcpa-mcpserver
#  3. POST tools/list -> assert all 6 tools advertised
#  4. POST tools/call validate_input_file with an in-container CSV ->
#     assert isError = false and n_genes correct
#  5. Stop the container
#
# Usage:
#   tests/protocol/docker-smoke.sh                 # uses rcpa-mcpserver:test
#   tests/protocol/docker-smoke.sh my-image:tag    # override image tag

set -euo pipefail

IMAGE="${1:-rcpa-mcpserver:test}"
CTR=rcpa-mcpserver-smoke
PORT=$(shuf -i 30000-50000 -n 1)
STATIC_PORT=$(shuf -i 30000-50000 -n 1)

echo "[smoke] starting container $CTR (image=$IMAGE, port=$PORT, static=$STATIC_PORT)"
docker rm -f "$CTR" >/dev/null 2>&1 || true
docker run -d --name "$CTR" \
  -e RCPA_HOST=0.0.0.0 \
  -e RCPA_STATIC_HOST=0.0.0.0 \
  -e RCPA_ALLOW_LOCAL_URIS=TRUE \
  -p "${PORT}:9004" \
  -p "${STATIC_PORT}:9005" \
  "$IMAGE"

# EXIT trap is set after we define SID_FILE so cleanup also clears it.

echo "[smoke] waiting for /mcp to accept initialize..."
for i in $(seq 1 90); do
  status=$(curl -s -o /dev/null -w '%{http_code}' \
    --connect-timeout 1 --max-time 3 \
    -H 'Content-Type: application/json' \
    -H 'Accept: application/json, text/event-stream' \
    -H 'Origin: http://localhost' \
    -X POST "http://localhost:${PORT}/mcp" \
    -d '{"jsonrpc":"2.0","id":0,"method":"initialize","params":{"protocolVersion":"2025-06-18","clientInfo":{"name":"probe","version":"0"},"capabilities":{}}}' \
    || echo 000)
  if [ "$status" = "200" ]; then
    echo "[smoke] server up after ${i}s (HTTP $status)"
    break
  fi
  if [ "$i" = "90" ]; then
    echo "[smoke] server did not come up after 90s (last HTTP $status); container logs:"
    docker logs "$CTR" 2>&1 | tail -40
    exit 1
  fi
  sleep 1
done

SID_FILE=$(mktemp)
trap 'rm -f "$SID_FILE"; docker rm -f "$CTR" >/dev/null 2>&1 || true' EXIT

post() {
  local body="$1"
  local sid=""
  [ -s "$SID_FILE" ] && sid=$(cat "$SID_FILE")
  if [ -n "$sid" ]; then
    curl -s --max-time 60 -X POST "http://localhost:${PORT}/mcp" \
      -H 'Content-Type: application/json' \
      -H 'Accept: application/json, text/event-stream' \
      -H 'Origin: http://localhost' \
      -H "Mcp-Session-Id: ${sid}" \
      -d "$body"
  else
    curl -s --max-time 60 -X POST "http://localhost:${PORT}/mcp" \
      -H 'Content-Type: application/json' \
      -H 'Accept: application/json, text/event-stream' \
      -H 'Origin: http://localhost' \
      -d "$body"
  fi
}

# Posts and writes the resulting Mcp-Session-Id header (if any) to
# $SID_FILE so subsequent post calls pick it up.
post_capture_session() {
  local body="$1"
  local headers_file
  headers_file=$(mktemp)
  local resp
  resp=$(curl -s -D "$headers_file" -X POST "http://localhost:${PORT}/mcp" \
    -H 'Content-Type: application/json' \
    -H 'Accept: application/json, text/event-stream' \
    -H 'Origin: http://localhost' \
    -d "$body")
  awk 'tolower($1) == "mcp-session-id:" { gsub(/\r/, ""); print $2; exit }' \
    "$headers_file" > "$SID_FILE"
  rm -f "$headers_file"
  echo "$resp"
}

echo "[smoke] step 1: initialize (captures Mcp-Session-Id)"
init=$(post_capture_session '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","clientInfo":{"name":"smoke","version":"0"},"capabilities":{}}}')
echo "$init" | jq -e '.result.serverInfo.name == "rcpa-mcpserver"' >/dev/null
echo "[smoke]   OK: serverInfo.name = rcpa-mcpserver, session=$(cat "$SID_FILE")"
# The MCP spec recommends sending notifications/initialized; the
# framework dispatches subsequent requests fine without it, and
# notifications keep the connection open for SSE in this server
# implementation - skip it for the smoke test.

echo "[smoke] step 2: tools/list"
tools=$(post '{"jsonrpc":"2.0","id":2,"method":"tools/list"}')
names=$(echo "$tools" | jq -r '.result.tools[].name' | sort | tr '\n' ' ')
expected="plot_results run_consensus_analysis run_de_analysis run_meta_analysis run_pathway_analysis validate_input_file"
if [ "$(echo $names | xargs)" != "$expected" ]; then
  echo "[smoke]   FAIL: tools mismatch"
  echo "         expected: $expected"
  echo "         got:      $names"
  exit 1
fi
echo "[smoke]   OK: 6 tools listed"

echo "[smoke] step 3: write a CSV inside the container and validate it"
docker exec "$CTR" sh -c 'printf "\"\",\"s1\",\"s2\"\n\"g1\",1,2\n\"g2\",3,4\n\"g3\",5,6\n" > /tmp/smoke.csv'
call=$(post '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"validate_input_file","arguments":{"file_uri":"file:///tmp/smoke.csv","file_type":"expression_matrix"}}}')
is_err=$(echo "$call" | jq -r '.result.isError // false')
if [ "$is_err" = "true" ]; then
  echo "[smoke]   FAIL: validate_input_file returned isError=true"
  echo "$call" | jq .
  exit 1
fi
n_genes=$(echo "$call" | jq -r '.result.content[0].text' | jq -r '.n_genes')
if [ "$n_genes" != "3" ]; then
  echo "[smoke]   FAIL: expected n_genes=3, got n_genes=$n_genes"
  echo "$call" | jq .
  exit 1
fi
echo "[smoke]   OK: validate_input_file returned n_genes=3"

echo "[smoke] step 3b: admin surface MUST NOT be mounted in default (auth off) mode"
admin_healthz=$(curl -s -o /dev/null -w '%{http_code}' \
  --max-time 3 -H 'Origin: http://localhost' \
  "http://localhost:${PORT}/admin/healthz" || echo 000)
if [ "$admin_healthz" = "200" ]; then
  echo "[smoke]   FAIL: /admin/healthz returned 200 with RCPA_AUTH=off"
  exit 1
fi
admin_ui=$(curl -s -o /dev/null -w '%{http_code}' \
  --max-time 3 -H 'Origin: http://localhost' \
  "http://localhost:${PORT}/admin/ui" || echo 000)
if [ "$admin_ui" = "200" ]; then
  echo "[smoke]   FAIL: /admin/ui returned 200 with RCPA_AUTH=off"
  exit 1
fi
echo "[smoke]   OK: /admin/* returns ${admin_healthz}/${admin_ui} (not 200)"

echo "[smoke] step 4: static server serves /results/"
docker exec "$CTR" sh -c 'mkdir -p /var/lib/rcpa/results/smoke && echo "hello" > /var/lib/rcpa/results/smoke/hi.txt'
content=$(curl -s "http://localhost:${STATIC_PORT}/results/smoke/hi.txt")
if [ "$content" != "hello" ]; then
  echo "[smoke]   FAIL: static server did not return file content (got: $content)"
  exit 1
fi
echo "[smoke]   OK: static server returned the file"

echo "[smoke] step 5: run real DE analysis (RCPA::runDEAnalysis via limma)"
# Stage the fixture CSVs inside the container.
docker cp inst/fixtures/small_expr.csv "$CTR:/tmp/small_expr.csv"
docker cp inst/fixtures/small_design.csv "$CTR:/tmp/small_design.csv"
de=$(post '{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"run_de_analysis","arguments":{"expression_uri":"file:///tmp/small_expr.csv","experiment_design_uri":"file:///tmp/small_design.csv","method":"limma","contrast":"Treatment - Control"}}}')
is_err=$(echo "$de" | jq -r '.result.isError // false')
if [ "$is_err" = "true" ]; then
  echo "[smoke]   FAIL: run_de_analysis returned isError"
  echo "$de" | jq -r '.result.content[0].text'
  exit 1
fi
result_type=$(echo "$de" | jq -r '.result.content[0].text' | jq -r '.result_type')
if [ "$result_type" != "de_result" ]; then
  echo "[smoke]   FAIL: expected result_type=de_result, got $result_type"
  echo "$de" | jq .
  exit 1
fi
csv_url=$(echo "$de" | jq -r '.result.content[1].uri')
echo "[smoke]   OK: DE produced $csv_url"

echo "[smoke] step 6: download DE result via static server"
# The container emits BASE_URL=http://localhost:9005/... - rewrite to the
# host-mapped port we exposed.
csv_path=$(echo "$csv_url" | sed 's|.*/results/|results/|')
status=$(curl -s -o /tmp/de-result.csv -w '%{http_code}' "http://localhost:${STATIC_PORT}/${csv_path}")
if [ "$status" != "200" ]; then
  echo "[smoke]   FAIL: download returned HTTP $status"
  exit 1
fi
n_lines=$(wc -l < /tmp/de-result.csv)
if [ "$n_lines" -lt 5 ]; then
  echo "[smoke]   FAIL: DE result CSV has $n_lines lines (expected >= 5)"
  cat /tmp/de-result.csv
  exit 1
fi
header=$(head -1 /tmp/de-result.csv)
echo "$header" | grep -q "logFC" || { echo "[smoke]   FAIL: missing logFC column"; exit 1; }
echo "$header" | grep -q "pFDR" || { echo "[smoke]   FAIL: missing pFDR column"; exit 1; }
rm -f /tmp/de-result.csv
echo "[smoke]   OK: DE result CSV ($n_lines lines) has logFC + pFDR columns"

echo "[smoke] all checks passed"
