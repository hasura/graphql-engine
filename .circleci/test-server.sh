#!/usr/bin/env bash
set -euo pipefail

echo "Running tests on node $CIRCLE_NODE_INDEX of $CIRCLE_NODE_TOTAL"

if [ -z "$SERVER_TEST_TO_RUN" ]; then
  echo 'Please specify $SERVER_TEST_TO_RUN'
  exit 1
else
  echo "Running test $SERVER_TEST_TO_RUN"
fi

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
  run_hge_with_args --database-url "$HASURA_GRAPHQL_DATABASE_URL" serve
  if [ -n "${HASURA_GRAPHQL_DATABASE_URL_2:-}" ] ; then
    run_hge_with_args --database-url "$HASURA_GRAPHQL_DATABASE_URL_2" serve --server-port 8081
    wait_for_port 8081
  fi
  wait_for_port 8080
}

source_data_sources_utils() {
  # Only source this file in the $SERVER_TEST_TO_RUN case branch it's used,
  # to avoid sourcing for every server job & test.
  # https://github.com/hasura/graphql-engine-mono/pull/1526#discussion_r661411538
  SCRIPTS_SOURCE=$CIRCLECI_FOLDER/../scripts
  source "$SCRIPTS_SOURCE/data-sources-util.sh"
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

PYTEST_ROOT="$CIRCLECI_FOLDER/../server/tests-py"

OUTPUT_FOLDER=${OUTPUT_FOLDER:-"$CIRCLECI_FOLDER/test-server-output"}
mkdir -p "$OUTPUT_FOLDER"

TEST_TYPE="no-auth"
HPCTIXFILE=""
TIX_FILE_INDEX="1"
TIX_FILES=""

cd $PYTEST_ROOT

# TODO(swann): false if not root
RUN_WEBHOOK_TESTS=true
if [ $EUID != 0 ] ; then
  echo -e "SKIPPING webhook based tests, as \nroot permission is required for running webhook tests (inorder to trust certificate authority)."
  RUN_WEBHOOK_TESTS=false
fi

for port in 8080 8081 9876 5592 5000 5001 5594
do
	fail_if_port_busy $port
done

echo -e "\nINFO: GraphQL Executable : $GRAPHQL_ENGINE"
echo -e "INFO: Logs Folder        : $OUTPUT_FOLDER\n"

# This seems to flake out relatively often; try a mirror if so.
# Might also need to disable ipv6 or use a longer --timeout
pip3 install -r requirements.txt ||\
pip3 install -i http://mirrors.digitalocean.com/pypi/web/simple --trusted-host mirrors.digitalocean.com   -r requirements.txt

(cd remote_schemas/nodejs && npm_config_loglevel=error npm ci)

mkdir -p "$OUTPUT_FOLDER/hpc"

export EVENT_WEBHOOK_HEADER="MyEnvValue"

export HGE_URL="http://localhost:8080"
export HGE_URL_2=""
if [ -n ${HASURA_GRAPHQL_DATABASE_URL_2:-} ] ; then
	HGE_URL_2="http://localhost:8081"
fi
export WEBHOOK_FROM_ENV="http://127.0.0.1:5592"
export SCHEDULED_TRIGGERS_WEBHOOK_DOMAIN="http://127.0.0.1:5594"
export HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES=true
export REMOTE_SCHEMAS_WEBHOOK_DOMAIN="http://127.0.0.1:5000"

export PYTEST_ADDOPTS="-vv"

HGE_PIDS=""
WH_PID=""
WHC_PID=""
GQL_SERVER_PID=""

trap stop_services ERR
trap stop_services INT

run_pytest_parallel() {
	trap stop_services ERR
	if [ -n "${HASURA_GRAPHQL_DATABASE_URL_2:-}" ] ; then
		set -x
		pytest --hge-urls "$HGE_URL" "${HGE_URL_2:-}" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" "${HASURA_GRAPHQL_DATABASE_URL_2:-}" -n 2 --dist=loadfile "$@"
		set +x
	else
		set -x
		pytest --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" -n 1 "$@"
		set +x
	fi
}

case "$SERVER_TEST_TO_RUN" in
  test-server-flags)
    if ! $CIRCLECI_FOLDER/test-server-flags.sh ; then
	    echo "Testing GraphQL server flags failed"
	    exit 1
    fi

    if ! $CIRCLECI_FOLDER/test-deprecated-server-flags.sh ; then
	    echo "Testing GraphQL deprecated server flags failed"
	    exit 1
    fi
    ;;

  haskell-tests)
    echo -e "\n$(time_elapsed): <########## RUN GRAPHQL-ENGINE HASKELL TESTS ###########################################>\n"
    "${GRAPHQL_ENGINE_TESTS:?}" postgres
    ;;

  no-auth)
    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITHOUT ADMIN SECRET ###########################################>\n"
    TEST_TYPE="no-auth"

    start_multiple_hge_servers

    run_pytest_parallel

    kill_hge_servers
    ;;

  admin-secret)
    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET #####################################>\n"
    TEST_TYPE="admin-secret"

    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
    

    start_multiple_hge_servers

    run_pytest_parallel --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET"

    kill_hge_servers
    ;;

  admin-secret-unauthorized-role)
    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND UNAUTHORIZED ROLE #####################################>\n"
    TEST_TYPE="admin-secret-unauthorized-role"

    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
    export HASURA_GRAPHQL_UNAUTHORIZED_ROLE="anonymous"
    

    run_hge_with_args serve

    wait_for_port 8080

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-unauthorized-role test_graphql_queries.py::TestUnauthorizedRolePermission

    kill_hge_servers

    #unset HASURA_GRAPHQL_UNAUTHORIZED_ROLE
    ;;

  jwt)
    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT #####################################>\n"
    TEST_TYPE="jwt"

    init_jwt

    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"    
    export HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat $OUTPUT_FOLDER/ssl/jwt_public.key)" '{ type: "RS512", key: $key }')"

    start_multiple_hge_servers

    run_pytest_parallel --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET"

    kill_hge_servers

    #unset HASURA_GRAPHQL_JWT_SECRET
    ;;

  jwt-stringified)
    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (in stringified mode) #####################################>\n"
    TEST_TYPE="jwt-stringified"

    init_jwt

    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
    
    export HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat $OUTPUT_FOLDER/ssl/jwt_public.key)" '{ type: "RS512", key: $key , claims_format: "stringified_json"}')"

    run_hge_with_args serve
    wait_for_port 8080

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET" test_jwt.py

    kill_hge_servers
    # unset HASURA_GRAPHQL_JWT_SECRET
    ;;

  jwt-audience-check-single-string)
    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (with audience check - string) #####################################>\n"
    TEST_TYPE="jwt-audience-check-single-string"

    init_jwt

    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
    
    export HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat $OUTPUT_FOLDER/ssl/jwt_public.key)" '{ type: "RS512", key: $key , audience: "myapp-1234"}')"

    run_hge_with_args serve
    wait_for_port 8080

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET" test_jwt.py

    kill_hge_servers

    #unset HASURA_GRAPHQL_JWT_SECRET
    ;;

  jwt-audience-check-list-string)
    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (with audience check - list of strings) #################################>\n"
    TEST_TYPE="jwt-audience-check-list-string"

    init_jwt

    export HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat $OUTPUT_FOLDER/ssl/jwt_public.key)" '{ type: "RS512", key: $key , audience: ["myapp-1234", "myapp-9876"]}')"
    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
    

    run_hge_with_args serve
    wait_for_port 8080

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET" test_jwt.py

    kill_hge_servers

    unset HASURA_GRAPHQL_JWT_SECRET
    ;;

  jwt-issuer-check)
    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (with issuer check) #####################################>\n"
    TEST_TYPE="jwt-issuer-check"

    init_jwt

    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
    
    export HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat $OUTPUT_FOLDER/ssl/jwt_public.key)" '{ type: "RS512", key: $key , issuer: "https://hasura.com"}')"

    run_hge_with_args serve
    wait_for_port 8080

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET" test_jwt.py

    kill_hge_servers

    unset HASURA_GRAPHQL_JWT_SECRET
    ;;

  jwt-with-claims-namespace-path)
    ##########
    # TODO(swann): should these not be run in parallel?
    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (with claims_namespace_path) #####################################>\n"
    TEST_TYPE="jwt-with-claims-namespace-path"

    init_jwt

    # hasura claims at one level of nesting
    export HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat $OUTPUT_FOLDER/ssl/jwt_public.key)" '{ type: "RS512", key: $key , claims_namespace_path: "$.hasura_claims"}')"
    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
    

    run_hge_with_args serve
    wait_for_port 8080

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET" test_jwt.py

    kill_hge_servers

    unset HASURA_GRAPHQL_JWT_SECRET

    # hasura claims at two levels of nesting with claims_namespace_path containing special character
    export HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat $OUTPUT_FOLDER/ssl/jwt_public.key)" '{ type: "RS512", key: $key , claims_namespace_path: "$.hasura['\''claims%'\'']"}')"
    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
    

    run_hge_with_args serve
    wait_for_port 8080

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET" test_jwt.py

    kill_hge_servers

    unset HASURA_GRAPHQL_JWT_SECRET

    # hasura claims at the root of the JWT token
    export HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat $OUTPUT_FOLDER/ssl/jwt_public.key)" '{ type: "RS512", key: $key , claims_namespace_path: "$"}')"
    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
    

    run_hge_with_args serve
    wait_for_port 8080

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET" test_jwt.py

    kill_hge_servers

    unset HASURA_GRAPHQL_JWT_SECRET
    ;;

  jwt-claims-map-with-json-path-values)
    # test JWT with Claims map
    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (with claims_map and values are json path) #####################################>\n"
    TEST_TYPE="jwt-claims-map-with-json-path-values"

    init_jwt

    export HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat $OUTPUT_FOLDER/ssl/jwt_public.key)" '{ type: "RS512", key: $key , claims_map: {"x-hasura-user-id": {"path":"$.['"'"'https://myapp.com/jwt/claims'"'"'].user.id"}, "x-hasura-allowed-roles": {"path":"$.['"'"'https://myapp.com/jwt/claims'"'"'].role.allowed"}, "x-hasura-default-role": {"path":"$.['"'"'https://myapp.com/jwt/claims'"'"'].role.default"}}}')"
    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
    

    run_hge_with_args serve
    wait_for_port 8080

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET" test_jwt_claims_map.py::TestJWTClaimsMapBasic

    kill_hge_servers

    unset HASURA_GRAPHQL_JWT_SECRET

    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (with claims_map and values are json path with default values set) #####################################>\n"
    TEST_TYPE="jwt-claims-map-with-json-path-values-with-default-values"

    export HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat $OUTPUT_FOLDER/ssl/jwt_public.key)" '{ type: "RS512", key: $key , claims_map: {"x-hasura-user-id": {"path":"$.['"'"'https://myapp.com/jwt/claims'"'"'].user.id", "default":"1"}, "x-hasura-allowed-roles": {"path":"$.['"'"'https://myapp.com/jwt/claims'"'"'].role.allowed", "default":["user","editor"]}, "x-hasura-default-role": {"path":"$.['"'"'https://myapp.com/jwt/claims'"'"'].role.default","default":"user"}}}')"
    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
    

    run_hge_with_args serve
    wait_for_port 8080

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET" test_jwt_claims_map.py::TestJWTClaimsMapBasic

    kill_hge_servers

    unset HASURA_GRAPHQL_JWT_SECRET
    ;;

  jwt-with-expiry-time-leeway)
    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (with JWT config allowing for leeway) #####################################>\n"
    TEST_TYPE="jwt-with-expiry-time-leeway"

    init_jwt
    export HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat $OUTPUT_FOLDER/ssl/jwt_public.key)" '{ type: "RS512", key: $key , allowed_skew: 60}')"
    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
    

    run_hge_with_args serve
    wait_for_port 8080

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET" test_jwt.py::TestJWTExpirySkew

    kill_hge_servers

    unset HASURA_GRAPHQL_JWT_SECRET
    ;;

  jwt-claims-map-with-literal-values)

    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (with claims_map and values are literal values) #####################################>\n"
    TEST_TYPE="jwt-claims-map-with-literal-values"

    init_jwt

    export HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat $OUTPUT_FOLDER/ssl/jwt_public.key)" '{ type: "RS512", key: $key , claims_map: {"x-hasura-user-id": {"path":"$.['"'"'https://myapp.com/jwt/claims'"'"'].user.id"}, "x-hasura-allowed-roles": ["user","editor"], "x-hasura-default-role": "user","x-hasura-custom-header":"custom-value"}}')"
    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
    

    run_hge_with_args serve
    wait_for_port 8080

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET" test_jwt_claims_map.py::TestJWTClaimsMapWithStaticHasuraClaimsMapValues

    kill_hge_servers

    unset HASURA_GRAPHQL_JWT_SECRET
    ;;

  jwt-cookie)

    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (in cookie mode) #####################################>\n"
    TEST_TYPE="jwt-cookie"

    init_jwt

    export HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat $OUTPUT_FOLDER/ssl/jwt_public.key)" '{ type: "RS512", key: $key , header: {"type": "Cookie", "name": "hasura_user"}}')"
    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
    

    run_hge_with_args serve
    wait_for_port 8080

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET" test_jwt.py

    kill_hge_servers

    unset HASURA_GRAPHQL_JWT_SECRET
    ;;

  # test with CORS modes
  cors-domains)
    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH CORS DOMAINS ########>\n"
    export HASURA_GRAPHQL_CORS_DOMAIN="http://*.localhost, http://localhost:3000, https://*.foo.bar.com"
    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
    
    TEST_TYPE="cors-domains"

    run_hge_with_args serve
    wait_for_port 8080

    pytest -n  1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-cors test_cors.py

    kill_hge_servers

    unset HASURA_GRAPHQL_CORS_DOMAIN
    ;;

  ws-init-cookie-read-cors-enabled)
    # test websocket transport with initial cookie header

    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH COOKIE IN WEBSOCKET INIT ########>\n"
    TEST_TYPE="ws-init-cookie-read-cors-enabled"
    export HASURA_GRAPHQL_AUTH_HOOK="http://localhost:9876/auth"
    export HASURA_GRAPHQL_AUTH_HOOK_MODE="POST"
    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

    python3 test_cookie_webhook.py > "$OUTPUT_FOLDER/cookie_webhook.log" 2>&1  & WHC_PID=$!
    wait_for_port 9876

    run_hge_with_args serve
    wait_for_port 8080

    echo "$(time_elapsed): testcase 1: read cookie, cors enabled"
    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-ws-init-cookie=read test_websocket_init_cookie.py

    kill_hge_servers
    ;;

  ws-init-cookie-noread)
    echo "$(time_elapsed): testcase 2: no read cookie, cors disabled"
    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
    
    TEST_TYPE="ws-init-cookie-noread"
    export HASURA_GRAPHQL_AUTH_HOOK="http://localhost:9876/auth"
    export HASURA_GRAPHQL_AUTH_HOOK_MODE="POST"
    run_hge_with_args serve --disable-cors

    wait_for_port 8080

    python3 test_cookie_webhook.py > "$OUTPUT_FOLDER/cookie_webhook.log" 2>&1  & WHC_PID=$!
    wait_for_port 9876

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-ws-init-cookie=noread test_websocket_init_cookie.py

    kill_hge_servers
    ;;

  ws-init-cookie-read-cors-disabled)
    echo "$(time_elapsed): testcase 3: read cookie, cors disabled and ws-read-cookie"
    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
    
    TEST_TYPE="ws-init-cookie-read-cors-disabled"
    export HASURA_GRAPHQL_AUTH_HOOK="http://localhost:9876/auth"
    export HASURA_GRAPHQL_AUTH_HOOK_MODE="POST"
    export HASURA_GRAPHQL_WS_READ_COOKIE="true"
    run_hge_with_args serve --disable-cors
    wait_for_port 8080

    python3 test_cookie_webhook.py > "$OUTPUT_FOLDER/cookie_webhook.log" 2>&1  & WHC_PID=$!
    wait_for_port 9876

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-ws-init-cookie=read test_websocket_init_cookie.py

    kill_hge_servers

    kill $WHC_PID
    # unset HASURA_GRAPHQL_WS_READ_COOKIE
    # unset HASURA_GRAPHQL_AUTH_HOOK
    # unset HASURA_GRAPHQL_AUTH_HOOK_MODE

    # required?
    # sleep 4
    ;;

  ws-graphql-api-disabled)
    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH GRAPHQL DISABLED ########>\n"
    TEST_TYPE="ws-graphql-api-disabled"
    export HASURA_GRAPHQL_ENABLED_APIS="metadata"
    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
    export HASURA_GRAPHQL_AUTH_HOOK="http://localhost:9876/auth"
    export HASURA_GRAPHQL_AUTH_HOOK_MODE="POST"

    run_hge_with_args serve
    wait_for_port 8080

    python3 test_cookie_webhook.py > "$OUTPUT_FOLDER/cookie_webhook.log" 2>&1  & WHC_PID=$!
    wait_for_port 9876

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-graphql-disabled test_apis_disabled.py

    kill_hge_servers

    unset HASURA_GRAPHQL_ENABLED_APIS

    run_hge_with_args serve --enabled-apis metadata
    wait_for_port 8080

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-graphql-disabled test_apis_disabled.py

    kill_hge_servers
    ;;

  ws-metadata-api-disabled)
    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH METADATA DISABLED ########>\n"
    TEST_TYPE="ws-metadata-api-disabled"

    export HASURA_GRAPHQL_ENABLED_APIS="graphql"
    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
    export HASURA_GRAPHQL_AUTH_HOOK="http://localhost:9876/auth"
    export HASURA_GRAPHQL_AUTH_HOOK_MODE="POST"

    run_hge_with_args serve
    wait_for_port 8080

    python3 test_cookie_webhook.py > "$OUTPUT_FOLDER/cookie_webhook.log" 2>&1  & WHC_PID=$!
    wait_for_port 9876

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-metadata-disabled test_apis_disabled.py

    kill_hge_servers
    unset HASURA_GRAPHQL_ENABLED_APIS

    run_hge_with_args serve --enabled-apis graphql
    wait_for_port 8080

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-metadata-disabled test_apis_disabled.py

    kill_hge_servers
    ;;

  remote-schema-permissions)
    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH REMOTE SCHEMA PERMISSIONS ENABLED ########>\n"
    TEST_TYPE="remote-schema-permissions"
    export HASURA_GRAPHQL_ENABLE_REMOTE_SCHEMA_PERMISSIONS=true
    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

    run_hge_with_args serve
    wait_for_port 8080

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET"  --enable-remote-schema-permissions test_remote_schema_permissions.py

    unset HASURA_GRAPHQL_ENABLE_REMOTE_SCHEMA_PERMISSIONS

    kill_hge_servers
    ;;

  function-permissions)
      echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH FUNCTION PERMISSIONS ENABLED ########>\n"
      TEST_TYPE="function-permissions"
      export HASURA_GRAPHQL_INFER_FUNCTION_PERMISSIONS=false
      export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

      run_hge_with_args serve
      wait_for_port 8080

      pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET"  --test-function-permissions test_graphql_queries.py::TestGraphQLQueryFunctionPermissions
      pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET"  --test-function-permissions test_graphql_mutations.py::TestGraphQLMutationFunctions

      unset HASURA_GRAPHQL_INFER_FUNCTION_PERMISSIONS
      unset HASURA_GRAPHQL_ADMIN_SECRET

      kill_hge_servers
      ;;

  inherited-roles)
      echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH EXPERIMENTAL FEATURE: INHERITED-ROLES ########>\n"
      TEST_TYPE="experimental-features-inherited-roles"
      export HASURA_GRAPHQL_EXPERIMENTAL_FEATURES="inherited_roles"
      export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

      run_hge_with_args serve
      wait_for_port 8080


      pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET"  --test-inherited-roles -k TestGraphQLInheritedRolesSchema
      pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET"  --test-inherited-roles -k TestGraphQLInheritedRolesPostgres
      pytest --hge-urls="$HGE_URL" --pg-urls="$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-inherited-roles test_graphql_mutations.py::TestGraphQLInheritedRoles

      unset HASURA_GRAPHQL_EXPERIMENTAL_FEATURES
      unset HASURA_GRAPHQL_ADMIN_SECRET

      kill_hge_servers
      ;;

  query-caching)
    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE QUERY CACHING #####################################>\n"
    TEST_TYPE="query-caching"
    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

    # use only one capability to disable cache striping
    run_hge_with_args +RTS -N1 -RTS serve
    wait_for_port 8080
    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" test_graphql_queries.py::TestGraphQLQueryCaching
    kill_hge_servers
    ;;

  query-logs)
    # verbose logging tests
    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH QUERY LOG ########>\n"
    TEST_TYPE="query-logs"
    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

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

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-logging test_logging.py

    unset HASURA_GRAPHQL_ENABLED_LOG_TYPES
    kill_hge_servers

    # end verbose logging tests
    ;;

  remote-schema-https)
      TEST_TYPE="remote-schemas-https"
      echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH SECURE REMOTE SCHEMA #########################>\n"

      export REMOTE_SCHEMAS_WEBHOOK_DOMAIN="https://127.0.0.1:5001/"
      init_ssl

      run_hge_with_args serve

      wait_for_port 8080

      python3 graphql_server.py 5001 "$OUTPUT_FOLDER/ssl/webhook.pem" "$OUTPUT_FOLDER/ssl/webhook-key.pem" > "$OUTPUT_FOLDER/remote_gql_server.log" 2>&1 & GQL_SERVER_PID=$!

      wait_for_port 5001

      pytest -n 1 --hge-urls="$HGE_URL" --pg-urls="$HASURA_GRAPHQL_DATABASE_URL" test_schema_stitching.py::TestRemoteSchemaBasic

      export REMOTE_SCHEMA_WEBHOOK_DOMAIN="https://localhost:5000/"
      kill_hge_servers
      kill $GQL_SERVER_PID
      ;;

  post-webhook)
    if [ "$RUN_WEBHOOK_TESTS" == "true" ] ; then
      TEST_TYPE="post-webhook"
      echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET & WEBHOOK (POST) #########################>\n"

      export HASURA_GRAPHQL_AUTH_HOOK="https://localhost:9090/"
      export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
      init_ssl

      start_multiple_hge_servers

      python3 webhook.py 9090 "$OUTPUT_FOLDER/ssl/webhook-key.pem" "$OUTPUT_FOLDER/ssl/webhook.pem" > "$OUTPUT_FOLDER/webhook.log" 2>&1  & WH_PID=$!
      wait_for_port 9090

      run_pytest_parallel --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-webhook="$HASURA_GRAPHQL_AUTH_HOOK"

      kill_hge_servers
    fi
    ;;

  webhook-request-context)
    if [ "$RUN_WEBHOOK_TESTS" == "true" ] ; then
      echo -e "\n$(time_elapsed): <########## TEST WEBHOOK RECEIVES REQUEST DATA AS CONTEXT #########################>\n"
      TEST_TYPE="webhook-request-context"
      export HASURA_GRAPHQL_AUTH_HOOK="http://localhost:5594/"
      export HASURA_GRAPHQL_AUTH_HOOK_MODE="POST"
      export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

      run_hge_with_args serve
      wait_for_port 8080

      pytest -s -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-webhook="$HASURA_GRAPHQL_AUTH_HOOK" --test-webhook-request-context test_webhook_request_context.py

      kill_hge_servers
    fi
    ;;


  get-webhook)
    if [ "$RUN_WEBHOOK_TESTS" == "true" ] ; then
      echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET & WEBHOOK (GET) #########################>\n"
      TEST_TYPE="get-webhook"
      export HASURA_GRAPHQL_AUTH_HOOK="https://localhost:9090/"
      export HASURA_GRAPHQL_AUTH_HOOK_MODE="POST"
      export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
      init_ssl

      start_multiple_hge_servers

      python3 webhook.py 9090 "$OUTPUT_FOLDER/ssl/webhook-key.pem" "$OUTPUT_FOLDER/ssl/webhook.pem" > "$OUTPUT_FOLDER/webhook.log" 2>&1  & WH_PID=$!
      wait_for_port 9090

      run_pytest_parallel --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-webhook="$HASURA_GRAPHQL_AUTH_HOOK"

      kill_hge_servers
    fi
    ;;

  insecure-webhook)
    if [ "$RUN_WEBHOOK_TESTS" == "true" ] ; then
      echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET & HTTPS INSECURE WEBHOOK (GET) ########>\n"
      TEST_TYPE="insecure-webhook"
      export HASURA_GRAPHQL_AUTH_HOOK="https://localhost:9090/"
      export HASURA_GRAPHQL_AUTH_HOOK_MODE="GET"
      export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
      init_ssl
      rm /etc/ssl/certs/webhook.crt
      update-ca-certificates

      run_hge_with_args serve
      wait_for_port 8080

      echo -e "running webhook"
      python3 webhook.py 9090 "$OUTPUT_FOLDER/ssl/webhook-key.pem" "$OUTPUT_FOLDER/ssl/webhook.pem" & WH_PID=$!
      echo -e "webhook pid $WH_PID"
      wait_for_port 9090

      pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-webhook="$HASURA_GRAPHQL_AUTH_HOOK" --test-webhook-insecure test_webhook_insecure.py

      kill_hge_servers
    fi
    ;;

  insecure-webhook-with-admin-secret)
    if [ "$RUN_WEBHOOK_TESTS" == "true" ] ; then
      echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN_SECRET & HTTPS INSECURE WEBHOOK WITH ADMIN SECRET (POST) ########>\n"
      TEST_TYPE="insecure-webhook-with-admin-secret"
      export HASURA_GRAPHQL_AUTH_HOOK="https://localhost:9090/"
      export HASURA_GRAPHQL_AUTH_HOOK_MODE="POST"
      export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
      init_ssl
      rm /etc/ssl/certs/webhook.crt
      update-ca-certificates

      run_hge_with_args serve
      wait_for_port 8080

      python3 webhook.py 9090 "$OUTPUT_FOLDER/ssl/webhook-key.pem" "$OUTPUT_FOLDER/ssl/webhook.pem" > "$OUTPUT_FOLDER/webhook.log" 2>&1  & WH_PID=$!
      echo -e "webhook pid $WH_PID"
      wait_for_port 9090

      pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-webhook="$HASURA_GRAPHQL_AUTH_HOOK" --test-webhook-insecure test_webhook_insecure.py

      kill_hge_servers

      kill $WH_PID
    fi
    ;;

  # TODO(swann): guard rest of the tests with RUN_WEBHOOK_TESTS either in each case or at toplevel
  allowlist-queries)
    # allowlist queries test
    # unset HASURA_GRAPHQL_AUTH_HOOK
    # unset HASURA_GRAPHQL_AUTH_HOOK_MODE
    # unset HASURA_GRAPHQL_JWT_SECRET
    # unset HASURA_GRAPHQL_ENABLE_ALLOWLIST

    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ALLOWLIST QUERIES ########> \n"
    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
    export HASURA_GRAPHQL_ENABLE_ALLOWLIST=true
    TEST_TYPE="allowlist-queries"

    run_hge_with_args serve
    wait_for_port 8080

    pytest -n  1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-allowlist-queries test_allowlist_queries.py

    kill_hge_servers
    unset HASURA_GRAPHQL_ENABLE_ALLOWLIST

    run_hge_with_args serve --enable-allowlist
    wait_for_port 8080

    pytest -n  1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-allowlist-queries test_allowlist_queries.py

    kill_hge_servers
    # end allowlist queries test
    ;;
  
  developer-api-tests)
    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH DEVELOPER API ENABLED ########>\n"
    TEST_TYPE="developer-api-tests"
    export HASURA_GRAPHQL_ENABLED_APIS="metadata,graphql,developer,config,pgdump"
    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

    run_hge_with_args serve --enabled-apis "$HASURA_GRAPHQL_ENABLED_APIS"
    wait_for_port 8080

    pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-developer-api-enabled test_dev_endpoints.py

    unset HASURA_GRAPHQL_ENABLED_APIS

    kill_hge_servers
    ;;

  jwk-url)
    # TODO(swann): ditto, these have to be parallelised

    # jwk test
    unset HASURA_GRAPHQL_AUTH_HOOK
    unset HASURA_GRAPHQL_AUTH_HOOK_MODE
    unset HASURA_GRAPHQL_JWT_SECRET

    export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
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

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-jwk-url test_jwk.py -k 'test_cache_control_header'

    kill_hge_servers
    unset HASURA_GRAPHQL_JWT_SECRET

    run_hge_with_args serve --jwt-secret "$cache_control_jwk_url"
    wait_for_port 8080

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-jwk-url test_jwk.py -k 'test_cache_control_header'

    kill_hge_servers

    # start HGE with expires JWK URL
    export HASURA_GRAPHQL_JWT_SECRET=$expires_jwk_url
    run_hge_with_args serve
    wait_for_port 8080

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-jwk-url test_jwk.py -k 'test_expires_header'

    kill_hge_servers
    unset HASURA_GRAPHQL_JWT_SECRET

    run_hge_with_args serve --jwt-secret "$expires_jwk_url"
    wait_for_port 8080

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-jwk-url test_jwk.py -k 'test_expires_header'

    kill_hge_servers

    # start HGE with nomaxage JWK URL
    export HASURA_GRAPHQL_JWT_SECRET=$cc_nomaxage_jwk_url
    run_hge_with_args serve
    wait_for_port 8080

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-jwk-url test_jwk.py -k 'test_cache_control_header'

    kill_hge_servers
    unset HASURA_GRAPHQL_JWT_SECRET

    # start HGE with nocache JWK URL
    export HASURA_GRAPHQL_JWT_SECRET=$cc_nocache_jwk_url
    run_hge_with_args serve
    wait_for_port 8080

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-jwk-url test_jwk.py -k 'test_cache_control_header'

    kill_hge_servers
    unset HASURA_GRAPHQL_JWT_SECRET

    kill $JWKS_PID

    # end jwk url test
    ;;

  horizontal-scaling)
    # horizontal scale test
    unset HASURA_GRAPHQL_AUTH_HOOK
    unset HASURA_GRAPHQL_AUTH_HOOK_MODE
    unset HASURA_GRAPHQL_ADMIN_SECRET

    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH HORIZONTAL SCALING ########>\n"
    TEST_TYPE="horizontal-scaling"

    HASURA_HS_TEST_DB='postgresql://postgres:postgres@localhost:6543/hs_hge_test'

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
    pytest --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --test-hge-scale-url="http://localhost:8081" test_horizontal_scale.py

    # Shutdown pgbouncer
    psql "postgresql://postgres:postgres@localhost:6543/pgbouncer" -c "SHUTDOWN;" || true

    cd $CIRCLECI_FOLDER

    # start pgbouncer again
    pgbouncer -u pgbouncer -d pgbouncer/pgbouncer.ini

    cd $PYTEST_ROOT

    # sleep for 20 seconds
    sleep 20

    # run test
    pytest --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --test-hge-scale-url="http://localhost:8081" test_horizontal_scale.py

    # Shutdown pgbouncer
    psql "postgresql://postgres:postgres@localhost:6543/pgbouncer" -c "SHUTDOWN;" || true

    kill_hge_servers

    psql "$HASURA_GRAPHQL_DATABASE_URL" -c "drop database hs_hge_test;"
    sleep 4
    unset HASURA_HS_TEST_DB

    # end horizontal scale test
    ;;
    # backend-* tests are excluded from `server-test-names.txt`
    # and are run via their respective `test_oss_server_*` circleci jobs
    backend-mssql)
    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH SQL SERVER BACKEND ###########################################>\n"
    TEST_TYPE="no-auth"
    export HASURA_GRAPHQL_EXPERIMENTAL_FEATURES="inherited_roles"

    run_hge_with_args serve
    wait_for_port 8080

    source_data_sources_utils
    add_mssql_source 8080 "$HASURA_GRAPHQL_MSSQL_SOURCE_URL"

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --backend mssql

    # start inherited roles test
    echo -e "\n$(time_elapsed): <########## TEST INHERITED-ROLES WITH SQL SERVER BACKEND ###########################################>\n"

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --test-inherited-roles -k TestGraphQLInheritedRolesMSSQL --backend mssql

    unset HASURA_GRAPHQL_EXPERIMENTAL_FEATURES
    # end inherited roles test

    kill_hge_servers
    ;;
    backend-citus)
    echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH CITUS BACKEND ###########################################>\n"
    TEST_TYPE="no-auth"

    run_hge_with_args serve
    wait_for_port 8080

    source_data_sources_utils
    add_citus_source 8080 "$HASURA_GRAPHQL_CITUS_SOURCE_URL"

    pytest -n 1 --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --backend citus

    kill_hge_servers
    ;;
esac

# FIXME(swann): we have to combine hpc reports outside this
# echo -e "\n$(time_elapsed): <########## COMBINE ALL HPC REPORTS ########>\n"
# generate_coverage_report || true

echo "Finished running tests on node $CIRCLE_NODE_INDEX of $CIRCLE_NODE_TOTAL"
echo -e "\n$(time_elapsed): <########## DONE ########>\n"
