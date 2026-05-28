#!/usr/bin/env bash
# Smoke-test the Docker container with RCPA_AUTH=on:
#
#   1. Start the container with the auth overlay; wait for /admin/healthz
#   2. Verify /mcp WITHOUT a bearer is 401 + WWW-Authenticate: Bearer
#   3. POST /admin/users (bootstrap) -> 201; capture user id
#   4. POST /admin/tokens/mint -> capture JWT
#   5. POST /mcp initialize + tools/list WITH the JWT -> 200
#   6. POST /admin/tokens/{jti}/revoke -> 204
#   7. Same JWT on /mcp -> 401
#   8. GET /admin/ui -> 200 + HTML, and /admin/ui/users falls back to index.html
#   9. Stop the container; restart with the same volume; bootstrap call lists
#      the previously-created user (persistence smoke).
#
# Usage:
#   tests/protocol/docker-smoke-auth.sh                  # uses rcpa-mcpserver:test
#   tests/protocol/docker-smoke-auth.sh my-image:tag

set -euo pipefail

IMAGE="${1:-rcpa-mcpserver:test}"
PROJECT="rcpa-mcpserver-smoke-auth"
PORT=$(shuf -i 30000-50000 -n 1)
STATIC_PORT=$(shuf -i 30000-50000 -n 1)
TOKEN=$(openssl rand -hex 32)

# Inline compose with a host-mapped port so we don't collide with other
# smoke runs on shared hosts.
WORKDIR=$(mktemp -d)
trap 'docker compose -p "$PROJECT" -f "$WORKDIR/compose.yaml" down -v >/dev/null 2>&1 || true; rm -rf "$WORKDIR"' EXIT

cat > "$WORKDIR/compose.yaml" <<EOF
services:
  rcpa-mcpserver:
    image: "$IMAGE"
    ports:
      - "${PORT}:9004"
      - "${STATIC_PORT}:9005"
    environment:
      RCPA_PORT: "9004"
      RCPA_STATIC_PORT: "9005"
      RCPA_HOST: "0.0.0.0"
      RCPA_STATIC_HOST: "0.0.0.0"
      RCPA_AUTH: "on"
      MCPSERVER_ADMIN_TOKEN: "$TOKEN"
      RCPA_AUTH_DB: "/var/lib/rcpa/auth/auth.db"
      # Allow file:// URIs so step 5b can validate an in-container CSV.
      # In production deployments leave this unset.
      RCPA_ALLOW_LOCAL_URIS: "TRUE"
    volumes:
      - rcpa-auth:/var/lib/rcpa/auth
volumes:
  rcpa-auth:
EOF

docker compose -p "$PROJECT" -f "$WORKDIR/compose.yaml" up -d
echo "[smoke-auth] waiting for /admin/healthz..."
for i in $(seq 1 90); do
  status=$(curl -s -o /dev/null -w '%{http_code}' \
    --max-time 3 -H "Authorization: Bearer $TOKEN" \
    -H 'Origin: http://localhost' \
    "http://localhost:${PORT}/admin/healthz" || echo 000)
  if [ "$status" = "200" ]; then
    echo "[smoke-auth] server up after ${i}s"
    break
  fi
  if [ "$i" = "90" ]; then
    echo "[smoke-auth] server did not come up after 90s (last HTTP $status); logs:"
    docker compose -p "$PROJECT" -f "$WORKDIR/compose.yaml" logs --tail 80
    exit 1
  fi
  sleep 1
done

# 2. /mcp without bearer -> 401 + WWW-Authenticate: Bearer
echo "[smoke-auth] step 2: /mcp without bearer must 401"
hdrs=$(mktemp)
status=$(curl -s -D "$hdrs" -o /dev/null -w '%{http_code}' \
  -X POST -H 'Content-Type: application/json' \
  -H 'Accept: application/json, text/event-stream' \
  -H 'Origin: http://localhost' \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}' \
  "http://localhost:${PORT}/mcp")
if [ "$status" != "401" ]; then
  echo "[smoke-auth]   FAIL: expected 401, got $status"
  exit 1
fi
www=$(awk 'tolower($1)=="www-authenticate:" {gsub(/\r/,""); $1=""; sub(/^ /,""); print; exit}' "$hdrs")
if ! echo "$www" | grep -qi "Bearer"; then
  echo "[smoke-auth]   FAIL: missing WWW-Authenticate: Bearer (got: $www)"
  exit 1
fi
rm -f "$hdrs"
echo "[smoke-auth]   OK: 401 + WWW-Authenticate: $www"

# 3. Create user via bootstrap token
echo "[smoke-auth] step 3: create user via /admin/users"
uname="alice-$$"
create=$(curl -s -X POST "http://localhost:${PORT}/admin/users" \
  -H "Authorization: Bearer $TOKEN" \
  -H 'Content-Type: application/json' \
  -H 'Origin: http://localhost' \
  -d "{\"username\":\"$uname\"}")
uid=$(echo "$create" | jq -r '.id')
if [ -z "$uid" ] || [ "$uid" = "null" ]; then
  echo "[smoke-auth]   FAIL: no user id; body=$create"
  exit 1
fi
echo "[smoke-auth]   OK: created user $uid"

# 4. Mint a JWT for that user
echo "[smoke-auth] step 4: mint a JWT"
mint=$(curl -s -X POST "http://localhost:${PORT}/admin/tokens/mint" \
  -H "Authorization: Bearer $TOKEN" \
  -H 'Content-Type: application/json' \
  -H 'Origin: http://localhost' \
  -d "{\"user_id\":\"$uid\",\"name\":\"smoke\",\"scopes\":[],\"ttl\":600}")
jti=$(echo "$mint" | jq -r '.jti')
jwt=$(echo "$mint" | jq -r '.token')
if [ -z "$jwt" ] || ! echo "$jwt" | grep -q '^eyJ'; then
  echo "[smoke-auth]   FAIL: no JWT in mint response; body=$mint"
  exit 1
fi
echo "[smoke-auth]   OK: minted jti=$jti"

# 5. Authorized initialize + tools/list
echo "[smoke-auth] step 5: /mcp WITH the JWT"
SID_FILE=$(mktemp)
trap 'rm -f "$SID_FILE"; docker compose -p "$PROJECT" -f "$WORKDIR/compose.yaml" down -v >/dev/null 2>&1 || true; rm -rf "$WORKDIR"' EXIT
hdrs=$(mktemp)
init=$(curl -s -D "$hdrs" -X POST "http://localhost:${PORT}/mcp" \
  -H "Authorization: Bearer $jwt" \
  -H 'Content-Type: application/json' \
  -H 'Accept: application/json, text/event-stream' \
  -H 'Origin: http://localhost' \
  -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{}}}')
sid=$(awk 'tolower($1)=="mcp-session-id:" {gsub(/\r/,""); print $2; exit}' "$hdrs")
rm -f "$hdrs"
if [ -z "$sid" ]; then
  echo "[smoke-auth]   FAIL: no Mcp-Session-Id; body=$init"
  exit 1
fi
echo "$sid" > "$SID_FILE"
tools=$(curl -s -X POST "http://localhost:${PORT}/mcp" \
  -H "Authorization: Bearer $jwt" \
  -H "Mcp-Session-Id: $sid" \
  -H 'Content-Type: application/json' \
  -H 'Accept: application/json, text/event-stream' \
  -H 'Origin: http://localhost' \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/list"}')
n_tools=$(echo "$tools" | jq -r '.result.tools | length')
if [ "$n_tools" != "6" ]; then
  echo "[smoke-auth]   FAIL: expected 6 tools, got $n_tools"
  echo "$tools" | jq .
  exit 1
fi
echo "[smoke-auth]   OK: 6 tools listed under JWT auth"

# 5b. Live tool call: validate_input_file with the JWT.
echo "[smoke-auth] step 5b: tools/call validate_input_file under JWT auth"
CTR=$(docker compose -p "$PROJECT" -f "$WORKDIR/compose.yaml" ps -q rcpa-mcpserver)
docker exec "$CTR" sh -c 'printf "\"\",\"s1\",\"s2\"\n\"g1\",1,2\n\"g2\",3,4\n\"g3\",5,6\n" > /tmp/smoke.csv'
call=$(curl -s -X POST "http://localhost:${PORT}/mcp" \
  -H "Authorization: Bearer $jwt" \
  -H "Mcp-Session-Id: $sid" \
  -H 'Content-Type: application/json' \
  -H 'Accept: application/json, text/event-stream' \
  -H 'Origin: http://localhost' \
  -d '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"validate_input_file","arguments":{"file_uri":"file:///tmp/smoke.csv","file_type":"expression_matrix"}}}')
is_err=$(echo "$call" | jq -r '.result.isError // false')
if [ "$is_err" = "true" ]; then
  echo "[smoke-auth]   FAIL: validate_input_file returned isError=true under JWT auth"
  echo "$call" | jq .
  exit 1
fi
n_genes=$(echo "$call" | jq -r '.result.content[0].text' | jq -r '.n_genes')
if [ "$n_genes" != "3" ]; then
  echo "[smoke-auth]   FAIL: expected n_genes=3 under JWT auth, got $n_genes"
  echo "$call" | jq .
  exit 1
fi
echo "[smoke-auth]   OK: validate_input_file returned n_genes=3 under JWT auth"

# 6. Revoke
echo "[smoke-auth] step 6: revoke the JWT"
rv_status=$(curl -s -o /dev/null -w '%{http_code}' \
  -X POST "http://localhost:${PORT}/admin/tokens/${jti}/revoke" \
  -H "Authorization: Bearer $TOKEN" \
  -H 'Origin: http://localhost')
if [ "$rv_status" != "204" ]; then
  echo "[smoke-auth]   FAIL: expected 204 from revoke, got $rv_status"
  exit 1
fi
echo "[smoke-auth]   OK: revoke returned 204"

# 7. Same JWT now 401
echo "[smoke-auth] step 7: revoked JWT must be 401"
dead=$(curl -s -o /dev/null -w '%{http_code}' \
  -X POST "http://localhost:${PORT}/mcp" \
  -H "Authorization: Bearer $jwt" \
  -H "Mcp-Session-Id: $sid" \
  -H 'Content-Type: application/json' \
  -H 'Accept: application/json, text/event-stream' \
  -H 'Origin: http://localhost' \
  -d '{"jsonrpc":"2.0","id":3,"method":"tools/list"}')
if [ "$dead" != "401" ]; then
  echo "[smoke-auth]   FAIL: expected 401 after revoke, got $dead"
  exit 1
fi
echo "[smoke-auth]   OK: revoked JWT returns 401"

# 8. SPA shell + fallback
echo "[smoke-auth] step 8: /admin/ui returns the SPA shell + deep-link fallback"
shell=$(curl -s "http://localhost:${PORT}/admin/ui")
if ! echo "$shell" | grep -q '/admin/ui/assets/'; then
  echo "[smoke-auth]   FAIL: /admin/ui shell missing asset references"
  echo "$shell"
  exit 1
fi
fallback=$(curl -s "http://localhost:${PORT}/admin/ui/users")
if ! echo "$fallback" | grep -q '<div id="root">'; then
  echo "[smoke-auth]   FAIL: /admin/ui/users deep-link did not fall back to index.html"
  exit 1
fi
echo "[smoke-auth]   OK: SPA shell + fallback"

# 9. Persistence across restart
echo "[smoke-auth] step 9: restart container; previously-created user must persist"
docker compose -p "$PROJECT" -f "$WORKDIR/compose.yaml" stop >/dev/null
docker compose -p "$PROJECT" -f "$WORKDIR/compose.yaml" up -d >/dev/null
for i in $(seq 1 60); do
  status=$(curl -s -o /dev/null -w '%{http_code}' \
    --max-time 3 -H "Authorization: Bearer $TOKEN" \
    -H 'Origin: http://localhost' \
    "http://localhost:${PORT}/admin/healthz" || echo 000)
  if [ "$status" = "200" ]; then break; fi
  sleep 1
done
list=$(curl -s -H "Authorization: Bearer $TOKEN" -H 'Origin: http://localhost' \
  "http://localhost:${PORT}/admin/users")
found=$(echo "$list" | jq -r --arg u "$uname" '.users[] | select(.username == $u) | .id')
if [ -z "$found" ]; then
  echo "[smoke-auth]   FAIL: user '$uname' did not survive restart; list=$list"
  exit 1
fi
echo "[smoke-auth]   OK: user '$uname' persisted (id=$found)"

echo "[smoke-auth] all checks passed"
