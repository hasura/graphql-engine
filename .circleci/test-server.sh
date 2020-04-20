#!/usr/bin/env bash
set -euo pipefail

### Functions

stop_services() {
   kill -s INT $HGE_PIDS || true
   kill $WH_PID || true
   kill -s INT $WHC_PID || true
}

time_elapsed(){
	printf "(%02d:%02d)" $[SECONDS/60] $[SECONDS%60]
}

fail_if_port_busy() {
    local PORT=$1
    if nc -z localhost $PORT ; then
        echo "Port $PORT is busy. Exiting"
        exit 1
    fi
}

wait_for_port() {
    local PORT=$1
    echo "waiting for $PORT"
    for _ in $(seq 1 60);
    do
      nc -z localhost $PORT && echo "port $PORT is ready" && return
      echo -n .
      sleep 0.25
    done
    echo "Failed waiting for $PORT" && exit 1
}

init_jwt() {
	CUR_DIR="$PWD"
	mkdir -p "$OUTPUT_FOLDER/ssl"
	cd "$OUTPUT_FOLDER/ssl"
	openssl genrsa -out jwt_private.key 2048
	openssl rsa -pubout -in jwt_private.key -out  jwt_public.key
	cd "$CUR_DIR"
}

init_ssl() {
	CUR_DIR="$PWD"
	mkdir -p "$OUTPUT_FOLDER/ssl"
	cd "$OUTPUT_FOLDER/ssl"
	CNF_TEMPLATE='[req]
req_extensions = v3_req
distinguished_name = req_distinguished_name

[req_distinguished_name]

[ v3_req ]
basicConstraints = CA:FALSE
keyUsage = nonRepudiation, digitalSignature, keyEncipherment
subjectAltName = @alt_names

[alt_names]
DNS.1 = localhost
IP.1 = 127.0.0.1'

	echo "$CNF_TEMPLATE" > webhook-req.cnf

	openssl genrsa -out ca-key.pem 2048
	openssl req -x509 -new -nodes -key ca-key.pem -days 10 -out ca.pem -subj "/CN=webhook-ca"
	openssl genrsa -out webhook-key.pem 2048
	openssl req -new -key webhook-key.pem -out webhook.csr -subj "/CN=hge-webhook" -config webhook-req.cnf
	openssl x509 -req -in webhook.csr -CA ca.pem -CAkey ca-key.pem -CAcreateserial -out webhook.pem -days 10 -extensions v3_req -extfile webhook-req.cnf

	cp ca.pem /etc/ssl/certs/webhook.crt
	update-ca-certificates
	cd "$CUR_DIR"
}

combine_all_hpc_reports() {
	combined_file="${OUTPUT_FOLDER}/graphql-engine.tix"
	combined_file_intermediate="${OUTPUT_FOLDER}/hpc/graphql-engine-combined-intermediate.tix"
	rm -f "$combined_file"
	IFS=: tix_files_arr=($TIX_FILES)
	unset IFS
	for tix_file in "${tix_files_arr[@]}"; do
		if ! [ -f "$tix_file" ]; then
			continue
		fi
		if [ -f "$combined_file" ]; then
      # Unset GHCRTS as hpc combine fails if GCHRTS=-N2 is present
			( unset GHCRTS
        set -x
        hpc combine "$combined_file" "$tix_file" --union --output="$combined_file_intermediate"
        set +x
        mv "$combined_file_intermediate" "$combined_file"
        rm "$tix_file"
      ) || true
		else
			mv "$tix_file" "$combined_file" || true
		fi
	done
}

generate_coverage_report() {
  combine_all_hpc_reports
  ( shopt -s failglob
    unset GHCRTS
    cd ~/graphql-engine/server
    mix_dirs=("$MIX_FILES_FOLDER"/*)
    # This is the bash syntax to prepend `--hpcdir=` to each element of an array. Yeah, I don’t like
    # it, either.
    hpcdir_args=("${mix_dirs[@]/#/--hpcdir=}")
    hpc_args=("${hpcdir_args[@]}" --reset-hpcdirs "$OUTPUT_FOLDER/graphql-engine.tix")
    hpc report "${hpc_args[@]}"
    mkdir -p "$OUTPUT_FOLDER/coverage"
    hpc markup "${hpc_args[@]}" --destdir="$OUTPUT_FOLDER/coverage" )
}

kill_hge_servers() {
	kill -s INT $HGE_PIDS || true
	wait $HGE_PIDS || true
	HGE_PIDS=""
}

run_hge_with_args() {
	i=$((TIX_FILE_INDEX++))
	export HPCTIXFILE="${OUTPUT_FOLDER}/hpc/graphql-engine-${i}-${TEST_TYPE}.tix"
	rm -f "$HPCTIXFILE"
	TIX_FILES="$TIX_FILES:$HPCTIXFILE"
	set -x
	"$GRAPHQL_ENGINE" "$@" 2>&1 > "$OUTPUT_FOLDER/graphql-engine-${i}-${TEST_TYPE}.log" & HGE_PIDS="$HGE_PIDS $!"
	set +x
}

start_multiple_hge_servers() {
	run_hge_with_args --database-url "$HASURA_GRAPHQL_DATABASE_URL" serve "$@"
	if [ -n ${HASURA_GRAPHQL_DATABASE_URL_2:-} ] ; then
		run_hge_with_args --database-url "$HASURA_GRAPHQL_DATABASE_URL_2" serve --server-port 8081 "$@"
		wait_for_port 8081
	fi
	wait_for_port 8080
}


if [ -z "${HASURA_GRAPHQL_DATABASE_URL:-}" ] ; then
	echo "Env var HASURA_GRAPHQL_DATABASE_URL is not set"
	exit 1
fi

if [ -z "${HASURA_GRAPHQL_DATABASE_URL_2:-}" ] ; then
	echo "Env var HASURA_GRAPHQL_DATABASE_URL_2 is not set"
	exit 1
fi

CIRCLECI_FOLDER="${BASH_SOURCE[0]%/*}"
cd $CIRCLECI_FOLDER
CIRCLECI_FOLDER="$PWD"

if ! $CIRCLECI_FOLDER/test-server-flags.sh ; then
	echo "Testing GraphQL server flags failed"
	exit 1
fi

if ! $CIRCLECI_FOLDER/test-deprecated-server-flags.sh ; then
	echo "Testing GraphQL deprecated server flags failed"
	exit 1
fi

PYTEST_ROOT="$CIRCLECI_FOLDER/../server/tests-py"

OUTPUT_FOLDER=${OUTPUT_FOLDER:-"$CIRCLECI_FOLDER/test-server-output"}
mkdir -p "$OUTPUT_FOLDER"

TEST_TYPE="no-auth"
HPCTIXFILE=""
TIX_FILE_INDEX="1"
TIX_FILES=""

cd $PYTEST_ROOT

RUN_WEBHOOK_TESTS=true

for port in 8080 8081 9876 5592
do
	fail_if_port_busy $port
done

echo -e "\nINFO: GraphQL Executable : $GRAPHQL_ENGINE"
echo -e "INFO: Logs Folder        : $OUTPUT_FOLDER\n"

pip3 install -r requirements.txt

mkdir -p "$OUTPUT_FOLDER/hpc"

export EVENT_WEBHOOK_HEADER="MyEnvValue"
export HGE_URL="http://localhost:8080"
export HGE_URL_2=""
if [ -n ${HASURA_GRAPHQL_DATABASE_URL_2:-} ] ; then
	HGE_URL_2="http://localhost:8081"
fi
export WEBHOOK_FROM_ENV="http://127.0.0.1:5592"
export HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES=true

HGE_PIDS=""
WH_PID=""
WHC_PID=""
HS_PID=""

trap stop_services ERR
trap stop_services INT

run_pytest_parallel() {
	trap stop_services ERR
	if [ -n ${HASURA_GRAPHQL_DATABASE_URL_2:-} ] ; then
		set -x
		pytest -vv --hge-urls "$HGE_URL" "${HGE_URL_2:-}" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" "${HASURA_GRAPHQL_DATABASE_URL_2:-}" -n 2 --dist=loadfile "$@"
		set +x
	else
		set -x
		pytest -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" -n 1 "$@"
		set +x
	fi
}

echo -e "\n$(time_elapsed): <########## RUN GRAPHQL-ENGINE HASKELL TESTS ###########################################>\n"
"${GRAPHQL_ENGINE_TESTS:?}" postgres

echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITHOUT ADMIN SECRET ###########################################>\n"
TEST_TYPE="no-auth"

start_multiple_hge_servers

run_pytest_parallel

kill_hge_servers

##########
echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET #####################################>\n"
TEST_TYPE="admin-secret"

export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

start_multiple_hge_servers

run_pytest_parallel --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET"

kill_hge_servers

##########
echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT #####################################>\n"
TEST_TYPE="jwt"

init_jwt

export HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat $OUTPUT_FOLDER/ssl/jwt_public.key)" '{ type: "RS512", key: $key }')"

start_multiple_hge_servers

run_pytest_parallel --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET"

kill_hge_servers

unset HASURA_GRAPHQL_JWT_SECRET

echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (in stringified mode) #####################################>\n"
TEST_TYPE="jwt-stringified"


export HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat $OUTPUT_FOLDER/ssl/jwt_public.key)" '{ type: "RS512", key: $key , claims_format: "stringified_json"}')"

run_hge_with_args serve
wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET" test_jwt.py

kill_hge_servers

unset HASURA_GRAPHQL_JWT_SECRET

echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (with audience check - string) #####################################>\n"
TEST_TYPE="jwt-audience-check-single-string"


export HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat $OUTPUT_FOLDER/ssl/jwt_public.key)" '{ type: "RS512", key: $key , audience: "myapp-1234"}')"

run_hge_with_args serve
wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET" test_jwt.py

kill_hge_servers

unset HASURA_GRAPHQL_JWT_SECRET

echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (with audience check - list of strings) #################################>\n"
TEST_TYPE="jwt-audience-check-list-string"

export HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat $OUTPUT_FOLDER/ssl/jwt_public.key)" '{ type: "RS512", key: $key , audience: ["myapp-1234", "myapp-9876"]}')"

run_hge_with_args serve
wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET" test_jwt.py

kill_hge_servers

unset HASURA_GRAPHQL_JWT_SECRET

echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (with issuer check) #####################################>\n"
TEST_TYPE="jwt-issuer-check"

export HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat $OUTPUT_FOLDER/ssl/jwt_public.key)" '{ type: "RS512", key: $key , issuer: "https://hasura.com"}')"

run_hge_with_args serve
wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET" test_jwt.py

kill_hge_servers

unset HASURA_GRAPHQL_JWT_SECRET

##########
echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (with claims_namespace_path) #####################################>\n"
TEST_TYPE="jwt-with-claims-namespace-path"

export HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat $OUTPUT_FOLDER/ssl/jwt_public.key)" '{ type: "RS512", key: $key , claims_namespace_path: "$.hasuraClaims"}')"

run_hge_with_args serve
wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET" test_jwt.py

kill_hge_servers

unset HASURA_GRAPHQL_JWT_SECRET

# test JWT with Claims map
echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (with claims_map) #####################################>\n"
TEST_TYPE="jwt-claims-map"

export HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat $OUTPUT_FOLDER/ssl/jwt_public.key)" '{ type: "RS512", key: $key , claims_map: {"x-hasura-user-id": "$.['"'"'https://myapp.com/jwt/claims'"'"'].user.id", "x-hasura-allowed-roles": "$.['"'"'https://myapp.com/jwt/claims'"'"'].role.allowed", "x-hasura-default-role": "$.['"'"'https://myapp.com/jwt/claims'"'"'].role.default"}}')"

run_hge_with_args serve
wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET" test_jwt_claims_map.py

kill_hge_servers

unset HASURA_GRAPHQL_JWT_SECRET

# test with CORS modes

echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH CORS DOMAINS ########>\n"
export HASURA_GRAPHQL_CORS_DOMAIN="http://*.localhost, http://localhost:3000, https://*.foo.bar.com"
TEST_TYPE="cors-domains"

run_hge_with_args serve
wait_for_port 8080

pytest -n  1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-cors test_cors.py

kill_hge_servers

unset HASURA_GRAPHQL_CORS_DOMAIN

# test websocket transport with initial cookie header

echo -e "\n$(time_elapsped): <########## TEST GRAPHQL-ENGINE WITH COOKIE IN WEBSOCKET INIT ########>\n"
TEST_TYPE="ws-init-cookie-read-cors-enabled"
export HASURA_GRAPHQL_AUTH_HOOK="http://localhost:9876/auth"
export HASURA_GRAPHQL_AUTH_HOOK_MODE="POST"

python3 test_cookie_webhook.py > "$OUTPUT_FOLDER/cookie_webhook.log" 2>&1  & WHC_PID=$!

wait_for_port 9876

run_hge_with_args serve
wait_for_port 8080

echo "$(time_elapsed): testcase 1: read cookie, cors enabled"
pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-ws-init-cookie=read test_websocket_init_cookie.py

kill_hge_servers

echo "$(time_elapsed): testcase 2: no read cookie, cors disabled"
TEST_TYPE="ws-init-cookie-noread"
run_hge_with_args serve --disable-cors

wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-ws-init-cookie=noread test_websocket_init_cookie.py

kill_hge_servers

echo "$(time_elapsed): testcase 3: read cookie, cors disabled and ws-read-cookie"
TEST_TYPE="ws-init-cookie-read-cors-disabled"
export HASURA_GRAPHQL_WS_READ_COOKIE="true"
run_hge_with_args serve --disable-cors
wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-ws-init-cookie=read test_websocket_init_cookie.py

kill_hge_servers

kill $WHC_PID
unset HASURA_GRAPHQL_WS_READ_COOKIE
unset HASURA_GRAPHQL_AUTH_HOOK
unset HASURA_GRAPHQL_AUTH_HOOK_MODE
sleep 4

echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH GRAPHQL DISABLED ########>\n"
TEST_TYPE="ws-graphql-api-disabled"
export HASURA_GRAPHQL_ENABLED_APIS="metadata"

run_hge_with_args serve
wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-graphql-disabled test_apis_disabled.py

kill_hge_servers

unset HASURA_GRAPHQL_ENABLED_APIS

run_hge_with_args serve --enabled-apis metadata
wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-graphql-disabled test_apis_disabled.py

kill_hge_servers

echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH METADATA DISABLED ########>\n"
TEST_TYPE="ws-metadata-api-disabled"

export HASURA_GRAPHQL_ENABLED_APIS="graphql"

run_hge_with_args serve
wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-metadata-disabled test_apis_disabled.py

kill_hge_servers
unset HASURA_GRAPHQL_ENABLED_APIS

run_hge_with_args serve --enabled-apis graphql
wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-metadata-disabled test_apis_disabled.py

kill_hge_servers

echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE QUERY CACHING #####################################>\n"
TEST_TYPE="query-caching"

# use only one capability to disable cache striping
run_hge_with_args +RTS -N1 -RTS serve
wait_for_port 8080
pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" test_graphql_queries.py::TestGraphQLQueryCaching
kill_hge_servers

# verbose logging tests
echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH QUERY LOG ########>\n"
TEST_TYPE="query-logs"

export HASURA_GRAPHQL_ENABLED_LOG_TYPES=" startup,http-log,webhook-log,websocket-log,query-log"

#run_hge_with_args serve
# we are doing this instead of calling run_hge_with_args, because we want to save in a custom log file
i=$((TIX_FILE_INDEX++))
export HPCTIXFILE="${OUTPUT_FOLDER}/hpc/graphql-engine-${i}-${TEST_TYPE}.tix"
rm -f "$HPCTIXFILE"
TIX_FILES="$TIX_FILES:$HPCTIXFILE"
set -x
export LOGGING_TEST_LOGFILE_PATH="$OUTPUT_FOLDER/graphql-engine-verbose-logging.log"
"$GRAPHQL_ENGINE" serve 2>&1 > "$LOGGING_TEST_LOGFILE_PATH" & HGE_PIDS="$HGE_PIDS $!"
set +x

wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-logging test_logging.py

unset HASURA_GRAPHQL_ENABLED_LOG_TYPES
kill_hge_servers

# end verbose logging tests

# webhook tests

if [ $EUID != 0 ] ; then
	echo -e "SKIPPING webhook based tests, as \nroot permission is required for running webhook tests (inorder to trust certificate authority)."
	RUN_WEBHOOK_TESTS=false
fi

if [ "$RUN_WEBHOOK_TESTS" == "true" ] ; then

	TEST_TYPE="post-webhook"
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET & WEBHOOK (GET) #########################>\n"

	export HASURA_GRAPHQL_AUTH_HOOK="https://localhost:9090/"
	init_ssl

	start_multiple_hge_servers

	python3 webhook.py 9090 "$OUTPUT_FOLDER/ssl/webhook-key.pem" "$OUTPUT_FOLDER/ssl/webhook.pem" > "$OUTPUT_FOLDER/webhook.log" 2>&1  & WH_PID=$!

	wait_for_port 9090

	run_pytest_parallel --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-webhook="$HASURA_GRAPHQL_AUTH_HOOK"

	kill_hge_servers

	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET & WEBHOOK (POST) #########################>\n"
	TEST_TYPE="get-webhook"
	export HASURA_GRAPHQL_AUTH_HOOK_MODE="POST"

	start_multiple_hge_servers

	run_pytest_parallel --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-webhook="$HASURA_GRAPHQL_AUTH_HOOK"

	rm /etc/ssl/certs/webhook.crt
	update-ca-certificates

	kill_hge_servers

	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET & HTTPS INSECURE WEBHOOK (GET) ########>\n"
	TEST_TYPE="insecure-webhook"
  	export HASURA_GRAPHQL_AUTH_HOOK_MODE="GET"

	run_hge_with_args serve

	wait_for_port 8080

	pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-webhook="$HASURA_GRAPHQL_AUTH_HOOK" --test-webhook-insecure test_webhook_insecure.py

	kill_hge_servers

	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN_SECRET & HTTPS INSECURE WEBHOOK (POST) ########>\n"
	TEST_TYPE="insecure-webhook-with-admin-secret"
  	export HASURA_GRAPHQL_AUTH_HOOK_MODE="POST"

	run_hge_with_args serve

	wait_for_port 8080

	pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-webhook="$HASURA_GRAPHQL_AUTH_HOOK" --test-webhook-insecure test_webhook_insecure.py

	kill_hge_servers

	kill $WH_PID

fi

# allowlist queries test
unset HASURA_GRAPHQL_AUTH_HOOK
unset HASURA_GRAPHQL_AUTH_HOOK_MODE
unset HASURA_GRAPHQL_JWT_SECRET
unset HASURA_GRAPHQL_ENABLE_ALLOWLIST

echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ALLOWLIST QUERIES ########> \n"
export HASURA_GRAPHQL_ENABLE_ALLOWLIST=true
TEST_TYPE="allowlist-queries"

run_hge_with_args serve
wait_for_port 8080

pytest -n  1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-allowlist-queries test_allowlist_queries.py

kill_hge_servers
unset HASURA_GRAPHQL_ENABLE_ALLOWLIST

run_hge_with_args serve --enable-allowlist
wait_for_port 8080

pytest -n  1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-allowlist-queries test_allowlist_queries.py

kill_hge_servers

# end allowlist queries test

# jwk test
unset HASURA_GRAPHQL_AUTH_HOOK
unset HASURA_GRAPHQL_AUTH_HOOK_MODE
unset HASURA_GRAPHQL_JWT_SECRET

echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH JWK URL ########> \n"
TEST_TYPE="jwk-url"

# start the JWK server
python3 jwk_server.py > "$OUTPUT_FOLDER/jwk_server.log" 2>&1  & JWKS_PID=$!
wait_for_port 5001

cache_control_jwk_url='{"jwk_url": "http://localhost:5001/jwk-cache-control"}'
expires_jwk_url='{"jwk_url": "http://localhost:5001/jwk-expires"}'
cc_nomaxage_jwk_url='{"jwk_url": "http://localhost:5001/jwk-cache-control?nomaxage"}'
cc_nocache_jwk_url='{"jwk_url": "http://localhost:5001/jwk-cache-control?nocache"}'

# start HGE with cache control JWK URL
export HASURA_GRAPHQL_JWT_SECRET=$cache_control_jwk_url
run_hge_with_args serve
wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-jwk-url test_jwk.py -k 'test_cache_control_header'

kill_hge_servers
unset HASURA_GRAPHQL_JWT_SECRET

run_hge_with_args serve --jwt-secret "$cache_control_jwk_url"
wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-jwk-url test_jwk.py -k 'test_cache_control_header'

kill_hge_servers

# start HGE with expires JWK URL
export HASURA_GRAPHQL_JWT_SECRET=$expires_jwk_url
run_hge_with_args serve
wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-jwk-url test_jwk.py -k 'test_expires_header'

kill_hge_servers
unset HASURA_GRAPHQL_JWT_SECRET

run_hge_with_args serve --jwt-secret "$expires_jwk_url"
wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-jwk-url test_jwk.py -k 'test_expires_header'

kill_hge_servers

# start HGE with nomaxage JWK URL
export HASURA_GRAPHQL_JWT_SECRET=$cc_nomaxage_jwk_url
run_hge_with_args serve
wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-jwk-url test_jwk.py -k 'test_cache_control_header'

kill_hge_servers
unset HASURA_GRAPHQL_JWT_SECRET

# start HGE with nocache JWK URL
export HASURA_GRAPHQL_JWT_SECRET=$cc_nocache_jwk_url
run_hge_with_args serve
wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-jwk-url test_jwk.py -k 'test_cache_control_header'

kill_hge_servers
unset HASURA_GRAPHQL_JWT_SECRET

kill $JWKS_PID

# end jwk url test

# horizontal scale test
unset HASURA_GRAPHQL_AUTH_HOOK
unset HASURA_GRAPHQL_AUTH_HOOK_MODE
unset HASURA_GRAPHQL_ADMIN_SECRET

echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH HORIZONTAL SCALING ########>\n"
TEST_TYPE="horizontal-scaling"

HASURA_HS_TEST_DB='postgres://postgres:postgres@localhost:6543/hs_hge_test'

if ! psql "$HASURA_GRAPHQL_DATABASE_URL" -c "SELECT 1 FROM pg_database WHERE datname = 'hs_hge_test'" | grep -q -F '(1 row)'
then
	psql "$HASURA_GRAPHQL_DATABASE_URL" -c 'CREATE DATABASE hs_hge_test;'
fi

pgUserInfo=$( python3 -c '
import os
from urllib.parse import urlparse
uri = urlparse( os.environ["HASURA_GRAPHQL_DATABASE_URL"] )
if uri.password:
    print("password="+uri.password+" user="+uri.username)
else:
    print("user="+uri.username)' )

pgDbInfo=$(psql "$HASURA_GRAPHQL_DATABASE_URL" -c "SELECT concat(' host=',inet_server_addr(),' port=', inet_server_port(),' dbname=',current_database())" | sed -n '3 p')

# create pgbouncer user
id pgbouncer || useradd pgbouncer
cd $CIRCLECI_FOLDER
mkdir -p pgbouncer
chown -R pgbouncer:pgbouncer pgbouncer

echo '[databases]
hs_hge_test = '"$pgDbInfo" "$pgUserInfo"'

[pgbouncer]
listen_port = 6543
listen_addr = 127.0.0.1
logfile = pgbouncer/pgbouncer.log
pidfile = pgbouncer/pgbouncer.pid
auth_type = md5
auth_file = pgbouncer/users.txt
admin_users = postgres' > pgbouncer/pgbouncer.ini

# start pgbouncer
pgbouncer -u pgbouncer -d pgbouncer/pgbouncer.ini

cd $PYTEST_ROOT
sleep 2

# start 1st server
run_hge_with_args --database-url "$HASURA_HS_TEST_DB" serve
wait_for_port 8080

# start 2nd server
run_hge_with_args --database-url "$HASURA_HS_TEST_DB" serve \
                  --server-port 8081
wait_for_port 8081

# run test
pytest -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --test-hge-scale-url="http://localhost:8081" test_horizontal_scale.py

# Shutdown pgbouncer
psql "postgres://postgres:postgres@localhost:6543/pgbouncer" -c "SHUTDOWN;" || true

cd $CIRCLECI_FOLDER

# start pgbouncer again
pgbouncer -u pgbouncer -d pgbouncer/pgbouncer.ini

cd $PYTEST_ROOT

# sleep for 20 seconds
sleep 20

# run test
pytest -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --test-hge-scale-url="http://localhost:8081" test_horizontal_scale.py

# Shutdown pgbouncer
psql "postgres://postgres:postgres@localhost:6543/pgbouncer" -c "SHUTDOWN;" || true

kill_hge_servers

psql "$HASURA_GRAPHQL_DATABASE_URL" -c "drop database hs_hge_test;"
sleep 4
unset HASURA_HS_TEST_DB

# end horizontal scale test

echo -e "\n$(time_elapsed): <########## COMBINE ALL HPC REPORTS ########>\n"
generate_coverage_report || true

echo -e "\n$(time_elapsed): <########## DONE ########>\n"
