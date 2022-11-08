#!/usr/bin/env bash
set -euo pipefail

echo "Running tests on node $CIRCLE_NODE_INDEX of $CIRCLE_NODE_TOTAL"

if [ -z "$SERVER_TEST_TO_RUN" ]; then
	# shellcheck disable=SC2016
	echo 'Please specify $SERVER_TEST_TO_RUN'
	exit 1
else
	echo "Running test $SERVER_TEST_TO_RUN"
fi

### Functions

# Disable the following warning:
# > Note that A && B || C is not if-then-else. C may run when A is true.
# We want this behavior, as we want to continue even if `kill` fails.
# shellcheck disable=SC2015
stop_services() {
	echo "killing and waiting for spawned services"

	[[ -n "${HGE_PIDS[*]}" ]] && {
		kill -s INT "${HGE_PIDS[@]}"
		wait "${HGE_PIDS[@]}"
	} || true
}

time_elapsed() {
	printf "(%02d:%02d)" $((SECONDS / 60)) $((SECONDS % 60))
}

fail_if_port_busy() {
	local PORT="$1"
	if nc -z localhost "$PORT"; then
		echo "ERROR:"
		echo "Port $PORT is busy."
		echo "Output of \`lsof -i :$PORT\`:"
		lsof -i ":$PORT"
		echo
		echo 'Exiting immediately.'
		exit 1
	fi
}

wait_for_port() {
	local PORT="$1"
	echo "waiting for $PORT"
	for _ in $(seq 1 60); do
		nc -z localhost "$PORT" && echo "port $PORT is ready" && return
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
	openssl rsa -pubout -in jwt_private.key -out jwt_public.key
	openssl genpkey -algorithm ed25519 -outform PEM -out ed25519_jwt_private.key
	openssl pkey -pubout -in ed25519_jwt_private.key -out ed25519_jwt_public.key
	cd "$CUR_DIR"
}

# init_hge_and_test_jwt function will run the hge server using the environment varibles and run the pytest which is sent as argument
# The first argument is the relative path of the jwt-key-file. the jwt-key-file can be RSA or EdDSA
# The second argument is the test to run, eg. test_jwt_claims_map.py::TestJWTClaimsMapBasic, test_jwt.py, etc.
init_hge_and_test_jwt() {
	local key_file
	key_file="${OUTPUT_FOLDER}/${1}"
	shift

	run_hge_with_args serve
	wait_for_port 8080
	pytest "${PYTEST_COMMON_ARGS[@]}" \
		--hge-jwt-key-file="$key_file" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET" \
		"$@"
	kill_hge_servers
}

init_ssl() {
	mkdir -p "$OUTPUT_FOLDER/ssl"
	(
		cd "$OUTPUT_FOLDER/ssl"
		openssl genrsa -out ca-key.pem 2048
		openssl req -x509 -new -nodes -key ca-key.pem -days 10 -out ca.pem -subj "/CN=webhook-ca"
		cp ca.pem /usr/local/share/ca-certificates/hge-tests-ca.crt
		update-ca-certificates
	)
}

webhook_tests_check_root() {
	if [ $EUID != 0 ]; then
		echo -e "webhook tests require root (in order to trust certificate authority)."
		exit 1
	fi
}

kill_hge_servers() {
	kill -s INT "${HGE_PIDS[@]}" || true
	wait "${HGE_PIDS[@]}" || true
	HGE_PIDS=()
}

HGE_INDEX=1
run_hge_with_args() {
	i=$((HGE_INDEX++))
	set -x
	"$GRAPHQL_ENGINE" "$@" >"$OUTPUT_FOLDER/graphql-engine-${i}.log" 2>&1 &
	HGE_PIDS=("${HGE_PIDS[@]}" $!)
	set +x
}

start_multiple_hge_servers() {
	run_hge_with_args --database-url "$HASURA_GRAPHQL_DATABASE_URL" serve
	if [ -n "${HASURA_GRAPHQL_DATABASE_URL_2:-}" ]; then
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
	# shellcheck source=../scripts/data-sources-util.sh
	source "$SCRIPTS_SOURCE/data-sources-util.sh"
}

if [ -z "${HASURA_GRAPHQL_DATABASE_URL:-}" ]; then
	echo "Env var HASURA_GRAPHQL_DATABASE_URL is not set"
	exit 1
fi

if [ -z "${HASURA_GRAPHQL_DATABASE_URL_2:-}" ]; then
	echo "Env var HASURA_GRAPHQL_DATABASE_URL_2 is not set"
	exit 1
fi

CIRCLECI_FOLDER="${BASH_SOURCE[0]%/*}"
cd "$CIRCLECI_FOLDER"
CIRCLECI_FOLDER="$PWD"

PYTEST_ROOT="$CIRCLECI_FOLDER/../server/tests-py"

OUTPUT_FOLDER=${OUTPUT_FOLDER:-"$CIRCLECI_FOLDER/test-server-output"}
mkdir -p "$OUTPUT_FOLDER"

cd "$PYTEST_ROOT"

for port in 8080 8081 9876 5592 5000 5001 5593 5594; do
	fail_if_port_busy $port
done

echo -e "\nINFO: GraphQL Executable : $GRAPHQL_ENGINE"
echo -e "INFO: Logs Folder        : $OUTPUT_FOLDER\n"

# Copy the node_modules directory installed in the Docker image here.
cp -R /deps/node_modules .

export EVENT_WEBHOOK_HEADER="MyEnvValue"

export HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES=true
export DEFAULT_HASURA_EXPERIMENTAL_FEATURES=streaming_subscriptions
export HASURA_GRAPHQL_EXPERIMENTAL_FEATURES=$DEFAULT_HASURA_EXPERIMENTAL_FEATURES

export HGE_URL="http://localhost:8080"
export HGE_URL_2=""
if [ -n "${HASURA_GRAPHQL_DATABASE_URL_2:-}" ]; then
	HGE_URL_2="http://localhost:8081"
fi

export EVENT_WEBHOOK_HEADER="MyEnvValue"
export EVENT_WEBHOOK_HANDLER="http://localhost:5592"
export ACTION_WEBHOOK_HANDLER="http://localhost:5593"
export SCHEDULED_TRIGGERS_WEBHOOK_DOMAIN="http://localhost:5594"
export REMOTE_SCHEMAS_WEBHOOK_DOMAIN="http://localhost:5000"
export GRAPHQL_SERVICE_HANDLER="http://localhost:4001"
export GRAPHQL_SERVICE_1="http://localhost:4020"
export GRAPHQL_SERVICE_2="http://localhost:4021"
export GRAPHQL_SERVICE_3="http://localhost:4022"

PYTEST_REPORTING_ARGS=(
	'-vv'     # show all passes and the full assertion text
	'-r' 'a'  # show extra test summary info for every test except passes
)

PYTEST_COMMON_ARGS=(
	"${PYTEST_REPORTING_ARGS[@]}"
	--hge-urls="$HGE_URL"
	--pg-urls="$HASURA_GRAPHQL_DATABASE_URL"
)

PYTEST_PARALLEL_ARGS=(
	"${PYTEST_REPORTING_ARGS[@]}"
	-n 2
	--hge-urls "$HGE_URL" "${HGE_URL_2:-}"
	--pg-urls "$HASURA_GRAPHQL_DATABASE_URL" "${HASURA_GRAPHQL_DATABASE_URL_2}"
)

HGE_PIDS=()

trap stop_services ERR
trap stop_services INT
trap stop_services EXIT

run_pytest_parallel() {
	trap stop_services ERR
	if [ -n "${HASURA_GRAPHQL_DATABASE_URL_2:-}" ]; then
		set -x
		pytest "${PYTEST_PARALLEL_ARGS[@]}" "$@"
		set +x
	else
		set -x
		pytest "${PYTEST_COMMON_ARGS[@]}" "$@"
		set +x
	fi
}

case "$SERVER_TEST_TO_RUN" in
haskell-tests)
	echo -e "\n$(time_elapsed): <########## RUN GRAPHQL-ENGINE HASKELL TESTS ###########################################>\n"
	"${GRAPHQL_ENGINE_TESTS:?}" postgres
	;;

no-auth)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITHOUT ADMIN SECRET ###########################################>\n"

	start_multiple_hge_servers

	run_pytest_parallel

	kill_hge_servers
	;;

admin-secret)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET #####################################>\n"

	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	start_multiple_hge_servers

	run_pytest_parallel

	kill_hge_servers
	;;

admin-secret-unauthorized-role)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND UNAUTHORIZED ROLE #####################################>\n"

	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
	export HASURA_GRAPHQL_UNAUTHORIZED_ROLE="anonymous"

	run_hge_with_args serve

	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		--test-unauthorized-role \
		test_graphql_queries.py::TestUnauthorizedRolePermission

	kill_hge_servers

	#unset HASURA_GRAPHQL_UNAUTHORIZED_ROLE
	;;

jwt-rs512)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (RS512) #####################################>\n"

	init_jwt

	HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/jwt_public.key")" '{ type: "RS512", key: $key }')"
	export HASURA_GRAPHQL_ADMIN_SECRET HASURA_GRAPHQL_JWT_SECRET

	start_multiple_hge_servers

	run_pytest_parallel --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET"

	kill_hge_servers

	#unset HASURA_GRAPHQL_JWT_SECRET
	;;

jwt-ed25519)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (Ed25519) #####################################>\n"

	init_jwt

	HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/ed25519_jwt_public.key")" '{ type: "Ed25519", key: $key }')"
	export HASURA_GRAPHQL_ADMIN_SECRET HASURA_GRAPHQL_JWT_SECRET

	start_multiple_hge_servers

	run_pytest_parallel --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/ed25519_jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET"

	kill_hge_servers

	#unset HASURA_GRAPHQL_JWT_SECRET
	;;

jwt-stringified)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (in stringified mode) #####################################>\n"

	init_jwt

	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/jwt_public.key")" '{ type: "RS512", key: $key , claims_format: "stringified_json"}')"
	export HASURA_GRAPHQL_JWT_SECRET

	init_hge_and_test_jwt "ssl/jwt_private.key" test_jwt.py

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/ed25519_jwt_public.key")" '{ type: "Ed25519", key: $key , claims_format: "stringified_json"}')"
	export HASURA_GRAPHQL_JWT_SECRET

	init_hge_and_test_jwt "ssl/ed25519_jwt_private.key" test_jwt.py

	# unset HASURA_GRAPHQL_JWT_SECRET
	;;

jwt-audience-check-single-string)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (with audience check - string) #####################################>\n"

	init_jwt

	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/jwt_public.key")" '{ type: "RS512", key: $key , audience: "myapp-1234"}')"
	export HASURA_GRAPHQL_JWT_SECRET

	init_hge_and_test_jwt "ssl/jwt_private.key" test_jwt.py

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/ed25519_jwt_public.key")" '{ type: "Ed25519", key: $key , audience: "myapp-1234"}')"
	export HASURA_GRAPHQL_JWT_SECRET

	init_hge_and_test_jwt "ssl/ed25519_jwt_private.key" test_jwt.py

	#unset HASURA_GRAPHQL_JWT_SECRET
	;;

jwt-audience-check-list-string)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (with audience check - list of strings) #################################>\n"

	init_jwt

	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/jwt_public.key")" '{ type: "RS512", key: $key , audience: ["myapp-1234", "myapp-9876"]}')"
	export HASURA_GRAPHQL_JWT_SECRET

	init_hge_and_test_jwt "ssl/jwt_private.key" test_jwt.py

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/ed25519_jwt_public.key")" '{ type: "Ed25519", key: $key , audience: ["myapp-1234", "myapp-9876"]}')"
	export HASURA_GRAPHQL_JWT_SECRET

	init_hge_and_test_jwt "ssl/ed25519_jwt_private.key" test_jwt.py

	unset HASURA_GRAPHQL_JWT_SECRET
	;;

jwt-issuer-check)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (with issuer check) #####################################>\n"

	init_jwt

	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/jwt_public.key")" '{ type: "RS512", key: $key , issuer: "https://hasura.com"}')"
	export HASURA_GRAPHQL_JWT_SECRET

	init_hge_and_test_jwt "ssl/jwt_private.key" test_jwt.py

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/ed25519_jwt_public.key")" '{ type: "Ed25519", key: $key , issuer: "https://hasura.com"}')"
	export HASURA_GRAPHQL_JWT_SECRET

	init_hge_and_test_jwt "ssl/ed25519_jwt_private.key" test_jwt.py

	unset HASURA_GRAPHQL_JWT_SECRET
	;;

jwt-with-claims-namespace-path)
	##########
	# TODO(swann): should these not be run in parallel?
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (with claims_namespace_path) #####################################>\n"

	init_jwt

	# hasura claims at one level of nesting
	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/jwt_public.key")" '{ type: "RS512", key: $key , claims_namespace_path: "$.hasura_claims"}')"
	export HASURA_GRAPHQL_JWT_SECRET

	init_hge_and_test_jwt "ssl/jwt_private.key" test_jwt.py

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/ed25519_jwt_public.key")" '{ type: "Ed25519", key: $key , claims_namespace_path: "$.hasura_claims"}')"
	export HASURA_GRAPHQL_JWT_SECRET

	init_hge_and_test_jwt "ssl/ed25519_jwt_private.key" test_jwt.py

	unset HASURA_GRAPHQL_JWT_SECRET

	# hasura claims at two levels of nesting with claims_namespace_path containing special character
	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/jwt_public.key")" '{ type: "RS512", key: $key , claims_namespace_path: "$.hasura['\''claims%'\'']"}')"
	export HASURA_GRAPHQL_JWT_SECRET

	init_hge_and_test_jwt "ssl/jwt_private.key" test_jwt.py

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/ed25519_jwt_public.key")" '{ type: "Ed25519", key: $key , claims_namespace_path: "$.hasura['\''claims%'\'']"}')"
	export HASURA_GRAPHQL_JWT_SECRET

	init_hge_and_test_jwt "ssl/ed25519_jwt_private.key" test_jwt.py

	unset HASURA_GRAPHQL_JWT_SECRET

	# hasura claims at the root of the JWT token
	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/jwt_public.key")" '{ type: "RS512", key: $key , claims_namespace_path: "$"}')"
	export HASURA_GRAPHQL_JWT_SECRET

	init_hge_and_test_jwt "ssl/jwt_private.key" test_jwt.py

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/ed25519_jwt_public.key")" '{ type: "Ed25519", key: $key , claims_namespace_path: "$"}')"
	export HASURA_GRAPHQL_JWT_SECRET

	init_hge_and_test_jwt "ssl/ed25519_jwt_private.key" test_jwt.py

	unset HASURA_GRAPHQL_JWT_SECRET
	;;

jwt-claims-map-with-json-path-values)
	# test JWT with Claims map
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (with claims_map and values are json path) #####################################>\n"

	init_jwt

	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/jwt_public.key")" '{ type: "RS512", key: $key , claims_map: {"x-hasura-user-id": {"path":"$.['"'"'https://myapp.com/jwt/claims'"'"'].user.id"}, "x-hasura-allowed-roles": {"path":"$.['"'"'https://myapp.com/jwt/claims'"'"'].role.allowed"}, "x-hasura-default-role": {"path":"$.['"'"'https://myapp.com/jwt/claims'"'"'].role.default"}}}')"
	export HASURA_GRAPHQL_JWT_SECRET

	init_hge_and_test_jwt "ssl/jwt_private.key" test_jwt_claims_map.py::TestJWTClaimsMapBasic

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/ed25519_jwt_public.key")" '{ type: "Ed25519", key: $key , claims_map: {"x-hasura-user-id": {"path":"$.['"'"'https://myapp.com/jwt/claims'"'"'].user.id"}, "x-hasura-allowed-roles": {"path":"$.['"'"'https://myapp.com/jwt/claims'"'"'].role.allowed"}, "x-hasura-default-role": {"path":"$.['"'"'https://myapp.com/jwt/claims'"'"'].role.default"}}}')"
	export HASURA_GRAPHQL_JWT_SECRET

	init_hge_and_test_jwt "ssl/ed25519_jwt_private.key" test_jwt_claims_map.py::TestJWTClaimsMapBasic

	unset HASURA_GRAPHQL_JWT_SECRET

	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (with claims_map and values are json path with default values set) #####################################>\n"

	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/jwt_public.key")" '{ type: "RS512", key: $key , claims_map: {"x-hasura-user-id": {"path":"$.['"'"'https://myapp.com/jwt/claims'"'"'].user.id", "default":"1"}, "x-hasura-allowed-roles": {"path":"$.['"'"'https://myapp.com/jwt/claims'"'"'].role.allowed", "default":["user","editor"]}, "x-hasura-default-role": {"path":"$.['"'"'https://myapp.com/jwt/claims'"'"'].role.default","default":"user"}}}')"
	export HASURA_GRAPHQL_JWT_SECRET

	init_hge_and_test_jwt "ssl/jwt_private.key" test_jwt_claims_map.py::TestJWTClaimsMapBasic

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/ed25519_jwt_public.key")" '{ type: "Ed25519", key: $key , claims_map: {"x-hasura-user-id": {"path":"$.['"'"'https://myapp.com/jwt/claims'"'"'].user.id", "default":"1"}, "x-hasura-allowed-roles": {"path":"$.['"'"'https://myapp.com/jwt/claims'"'"'].role.allowed", "default":["user","editor"]}, "x-hasura-default-role": {"path":"$.['"'"'https://myapp.com/jwt/claims'"'"'].role.default","default":"user"}}}')"
	export HASURA_GRAPHQL_JWT_SECRET

	init_hge_and_test_jwt "ssl/ed25519_jwt_private.key" test_jwt_claims_map.py::TestJWTClaimsMapBasic

	unset HASURA_GRAPHQL_JWT_SECRET
	;;

jwt-with-expiry-time-leeway)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (with JWT config allowing for leeway) #####################################>\n"

	init_jwt

	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/jwt_public.key")" '{ type: "RS512", key: $key , allowed_skew: 60}')"
	export HASURA_GRAPHQL_JWT_SECRET

	init_hge_and_test_jwt "ssl/jwt_private.key" test_jwt.py::TestJWTExpirySkew

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/ed25519_jwt_public.key")" '{ type: "Ed25519", key: $key , allowed_skew: 60}')"
	export HASURA_GRAPHQL_JWT_SECRET

	init_hge_and_test_jwt "ssl/ed25519_jwt_private.key" test_jwt.py::TestJWTExpirySkew

	unset HASURA_GRAPHQL_JWT_SECRET
	;;

jwt-claims-map-with-literal-values)

	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (with claims_map and values are literal values) #####################################>\n"

	init_jwt

	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/jwt_public.key")" '{ type: "RS512", key: $key , claims_map: {"x-hasura-user-id": {"path":"$.['"'"'https://myapp.com/jwt/claims'"'"'].user.id"}, "x-hasura-allowed-roles": ["user","editor"], "x-hasura-default-role": "user","x-hasura-custom-header":"custom-value"}}')"
	export HASURA_GRAPHQL_JWT_SECRET

	init_hge_and_test_jwt "ssl/jwt_private.key" test_jwt_claims_map.py::TestJWTClaimsMapWithStaticHasuraClaimsMapValues

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/ed25519_jwt_public.key")" '{ type: "Ed25519", key: $key , claims_map: {"x-hasura-user-id": {"path":"$.['"'"'https://myapp.com/jwt/claims'"'"'].user.id"}, "x-hasura-allowed-roles": ["user","editor"], "x-hasura-default-role": "user","x-hasura-custom-header":"custom-value"}}')"
	export HASURA_GRAPHQL_JWT_SECRET

	init_hge_and_test_jwt "ssl/ed25519_jwt_private.key" test_jwt_claims_map.py::TestJWTClaimsMapWithStaticHasuraClaimsMapValues

	unset HASURA_GRAPHQL_JWT_SECRET
	;;

jwt-cookie)

	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (in cookie mode) #####################################>\n"

	init_jwt

	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/jwt_public.key")" '{ type: "RS512", key: $key , header: {"type": "Cookie", "name": "hasura_user"}}')"
	export HASURA_GRAPHQL_JWT_SECRET

	init_hge_and_test_jwt "ssl/jwt_private.key" test_jwt.py

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/ed25519_jwt_public.key")" '{ type: "Ed25519", key: $key , header: {"type": "Cookie", "name": "hasura_user"}}')"
	export HASURA_GRAPHQL_JWT_SECRET

	init_hge_and_test_jwt "ssl/ed25519_jwt_private.key" test_jwt.py

	unset HASURA_GRAPHQL_JWT_SECRET
	;;

jwt-cookie-unauthorized-role)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH JWT (in cookie mode) AND UNAUTHORIZED ROLE #####################################>\n"

	init_jwt

	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
	export HASURA_GRAPHQL_UNAUTHORIZED_ROLE="anonymous"

	HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat "$OUTPUT_FOLDER/ssl/jwt_public.key")" '{ type: "RS512", key: $key , header: {"type": "Cookie", "name": "hasura_user"}}')"
	export HASURA_GRAPHQL_JWT_SECRET

	run_hge_with_args serve

	wait_for_port 8080
	pytest "${PYTEST_COMMON_ARGS[@]}" \
		--hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET" \
		--test-unauthorized-role \
		test_graphql_queries.py::TestFallbackUnauthorizedRoleCookie

	kill_hge_servers

	unset HASURA_GRAPHQL_UNAUTHORIZED_ROLE

	run_hge_with_args serve

	wait_for_port 8080
	pytest "${PYTEST_COMMON_ARGS[@]}" \
		--hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET" \
		--test-no-cookie-and-unauth-role \
		test_graphql_queries.py::TestMissingUnauthorizedRoleAndCookie

	kill_hge_servers

	unset HASURA_GRAPHQL_JWT_SECRET
	;;

# test with CORS modes
cors-domains)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH CORS DOMAINS ########>\n"
	export HASURA_GRAPHQL_CORS_DOMAIN="http://*.localhost, http://localhost:3000, https://*.foo.bar.com"
	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	run_hge_with_args serve
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		test_cors.py

	kill_hge_servers

	unset HASURA_GRAPHQL_CORS_DOMAIN
	;;

auth-webhook-cookie)
	# test auth webhook set-cookie forwarding on response

	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH SET-COOKIE HEADER IN AUTH WEBHOOK ########>\n"
	export HASURA_GRAPHQL_AUTH_HOOK="http://localhost:9876/auth"
	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	run_hge_with_args serve
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		test_auth_webhook_cookie.py

	kill_hge_servers
	;;

ws-init-cookie-read-cors-enabled)
	# test websocket transport with initial cookie header

	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH COOKIE IN WEBSOCKET INIT ########>\n"
	export HASURA_GRAPHQL_AUTH_HOOK="http://localhost:9876/auth"
	export HASURA_GRAPHQL_AUTH_HOOK_MODE="POST"
	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	run_hge_with_args serve
	wait_for_port 8080

	echo "$(time_elapsed): testcase 1: read cookie, cors enabled"
	pytest "${PYTEST_COMMON_ARGS[@]}" \
		--test-ws-init-cookie=read \
		test_websocket_init_cookie.py

	kill_hge_servers
	;;

ws-init-cookie-noread)
	echo "$(time_elapsed): testcase 2: no read cookie, cors disabled"
	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	export HASURA_GRAPHQL_AUTH_HOOK="http://localhost:9876/auth"
	export HASURA_GRAPHQL_AUTH_HOOK_MODE="POST"
	run_hge_with_args serve --disable-cors

	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		--test-ws-init-cookie=noread \
		test_websocket_init_cookie.py

	kill_hge_servers
	;;

ws-init-cookie-read-cors-disabled)
	echo "$(time_elapsed): testcase 3: read cookie, cors disabled and ws-read-cookie"
	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	export HASURA_GRAPHQL_AUTH_HOOK="http://localhost:9876/auth"
	export HASURA_GRAPHQL_AUTH_HOOK_MODE="POST"
	export HASURA_GRAPHQL_WS_READ_COOKIE="true"
	run_hge_with_args serve --disable-cors
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		--test-ws-init-cookie=read \
		test_websocket_init_cookie.py

	kill_hge_servers
	;;

ws-graphql-api-disabled)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH GRAPHQL DISABLED ########>\n"
	export HASURA_GRAPHQL_ENABLED_APIS="metadata"
	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
	export HASURA_GRAPHQL_AUTH_HOOK="http://localhost:9876/auth"
	export HASURA_GRAPHQL_AUTH_HOOK_MODE="POST"

	run_hge_with_args serve
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		test_apis_disabled.py

	kill_hge_servers
	;;

ws-metadata-api-disabled)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH METADATA DISABLED ########>\n"

	export HASURA_GRAPHQL_ENABLED_APIS="graphql"
	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
	export HASURA_GRAPHQL_AUTH_HOOK="http://localhost:9876/auth"
	export HASURA_GRAPHQL_AUTH_HOOK_MODE="POST"

	run_hge_with_args serve
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		test_apis_disabled.py

	kill_hge_servers
	;;

remote-schema-permissions)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH REMOTE SCHEMA PERMISSIONS ENABLED ########>\n"
	export HASURA_GRAPHQL_ENABLE_REMOTE_SCHEMA_PERMISSIONS=true
	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	run_hge_with_args serve
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		test_remote_schema_permissions.py

	unset HASURA_GRAPHQL_ENABLE_REMOTE_SCHEMA_PERMISSIONS

	kill_hge_servers
	;;

function-permissions)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH FUNCTION PERMISSIONS ENABLED ########>\n"
	export HASURA_GRAPHQL_INFER_FUNCTION_PERMISSIONS=false
	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	run_hge_with_args serve
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		test_graphql_queries.py::TestGraphQLQueryFunctionPermissions
	pytest "${PYTEST_COMMON_ARGS[@]}" \
		test_graphql_mutations.py::TestGraphQLMutationFunctions

	unset HASURA_GRAPHQL_INFER_FUNCTION_PERMISSIONS
	unset HASURA_GRAPHQL_ADMIN_SECRET

	kill_hge_servers
	;;

roles-inheritance)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH EXPERIMENTAL FEATURE: ROLES INHERITANCE ########>\n"

	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
	export HASURA_GRAPHQL_ENABLE_REMOTE_SCHEMA_PERMISSIONS="true"
	export HASURA_GRAPHQL_INFER_FUNCTION_PERMISSIONS=false

	run_hge_with_args serve
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		test_roles_inheritance.py

	unset HASURA_GRAPHQL_ADMIN_SECRET
	unset HASURA_GRAPHQL_ENABLE_REMOTE_SCHEMA_PERMISSIONS
	unset HASURA_GRAPHQL_INFER_FUNCTION_PERMISSIONS

	kill_hge_servers
	;;

naming-conventions)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH EXPERIMENTAL FEATURE: NAMING CONVENTIONS ########>\n"

	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM"
	run_hge_with_args serve
	wait_for_port 8080

	unset HASURA_GRAPHQL_EXPERIMENTAL_FEATURES

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		test_naming_conventions.py

	kill_hge_servers

	export HASURA_GRAPHQL_EXPERIMENTAL_FEATURES=naming_convention
	run_hge_with_args serve
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		test_naming_conventions.py

	kill_hge_servers

	# We are now going to test by setting the default naming convention to
	# graphql-default. So now we don't need to set the naming convention in
	# source customisation
	export HASURA_GRAPHQL_DEFAULT_NAMING_CONVENTION="graphql-default"

	run_hge_with_args serve
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		test_naming_conventions.py

	unset HASURA_GRAPHQL_ADMIN_SECRET
	export HASURA_GRAPHQL_EXPERIMENTAL_FEATURES=$DEFAULT_HASURA_EXPERIMENTAL_FEATURES
	unset HASURA_GRAPHQL_DEFAULT_NAMING_CONVENTION

	kill_hge_servers
	;;

streaming-subscriptions)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH STREAMING SUBSCRIPTIONS #########################>\n"

  	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	run_hge_with_args serve
	wait_for_port 8080

  	# run all the subscriptions tests with streaming subscriptions enabled
	pytest "${PYTEST_COMMON_ARGS[@]}" \
		test_subscriptions.py

  	# run test `TestSubscriptionBasicNoAuth` in case of no authentication for Apollo-ws protocol
	pytest "${PYTEST_COMMON_ARGS[@]}" \
		test_subscriptions.py::TestSubscriptionBasicNoAuth

	unset HASURA_GRAPHQL_ADMIN_SECRET
	unset HASURA_GRAPHQL_EXPERIMENTAL_FEATURES

	kill_hge_servers
	;;

query-caching)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE QUERY CACHING #####################################>\n"
	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	# use only one capability to disable cache striping
	run_hge_with_args +RTS -N1 -RTS serve
	wait_for_port 8080
	pytest "${PYTEST_COMMON_ARGS[@]}" \
		test_graphql_queries.py::TestGraphQLQueryCaching
	kill_hge_servers
	;;

query-logs)
	# verbose logging tests
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH QUERY LOG ########>\n"
	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	export HASURA_GRAPHQL_ENABLED_LOG_TYPES=" startup,http-log,webhook-log,websocket-log,query-log"
	export HASURA_GRAPHQL_LOG_LEVEL="debug"

	#run_hge_with_args serve
	# we are doing this instead of calling run_hge_with_args, because we want to save in a custom log file
	set -x
	export LOGGING_TEST_LOGFILE_PATH="$OUTPUT_FOLDER/graphql-engine-verbose-logging.log"
	"$GRAPHQL_ENGINE" serve >"$LOGGING_TEST_LOGFILE_PATH" 2>&1 &
	HGE_PIDS=("${HGE_PIDS[@]}" $!)
	set +x

	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		--test-logging \
		test_logging.py

	unset HASURA_GRAPHQL_ENABLED_LOG_TYPES
	kill_hge_servers

	# configurable jwk-refresh-log test
	unset HASURA_GRAPHQL_AUTH_HOOK
	unset HASURA_GRAPHQL_AUTH_HOOK_MODE
	unset HASURA_GRAPHQL_JWT_SECRET

	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH JWK REFRESH LOG ########> \n"

	export JWK_SERVER_URL='http://localhost:5001'

	# Start the JWK server.
	# There is a fixture to do this, but when running in this fashion, we need to
	# start the JWK server first so the HGE server can communicate with it.
	python3 jwk_server.py >"$OUTPUT_FOLDER/configurable_log.log" 2>&1 &
	JWKS_PID=$!
	wait_for_port 5001

	echo "Test: jwk-refresh-log type logs is enabled by default"
	export HASURA_GRAPHQL_JWT_SECRET="{\"jwk_url\": \"${JWK_SERVER_URL}/jwk-cache-control?no-cache=true\"}"

	#run_hge_with_args serve
	# we are doing this instead of calling run_hge_with_args, because we want to save in a custom log file
	set -x
	export LOGGING_TEST_LOGFILE_PATH="$OUTPUT_FOLDER/graphql-engine-verbose-logging.log"
	"$GRAPHQL_ENGINE" serve >"$LOGGING_TEST_LOGFILE_PATH" 2>&1 &
	HGE_PIDS=("${HGE_PIDS[@]}" $!)
	set +x

	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		--test-logging \
		test_logging.py::TestConfiguragbleLogs

	kill_hge_servers

	echo "Test: no jwk-refresh-log type logs if it is not enabled"
	export HASURA_GRAPHQL_ENABLED_LOG_TYPES=" startup,http-log,webhook-log,websocket-log,query-log"
	#run_hge_with_args serve
	# we are doing this instead of calling run_hge_with_args, because we want to save in a custom log file
	set -x
	export LOGGING_TEST_LOGFILE_PATH="$OUTPUT_FOLDER/graphql-engine-verbose-logging.log"
	"$GRAPHQL_ENGINE" serve >"$LOGGING_TEST_LOGFILE_PATH" 2>&1 &
	HGE_PIDS=("${HGE_PIDS[@]}" $!)
	set +x

	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		--test-logging \
		test_logging.py::TestConfiguragbleLogs

	kill_hge_servers
	unset HASURA_GRAPHQL_ENABLED_LOG_TYPES
	unset HASURA_GRAPHQL_JWT_SECRET
	unset JWK_SERVER_URL

	kill $JWKS_PID

	# end verbose logging tests
	;;

startup-db-calls)
	# verbose logging tests
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE STARTUP DB CALLS ########>\n"
	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	export HASURA_GRAPHQL_ENABLED_LOG_TYPES=" startup,http-log,webhook-log,websocket-log,query-log"
	export HASURA_GRAPHQL_LOG_LEVEL="debug"

	#run_hge_with_args serve
	# we are doing this instead of calling run_hge_with_args, because we want to save in a custom log file
	set -x
	export LOGGING_TEST_LOGFILE_PATH="$OUTPUT_FOLDER/graphql-engine-verbose-logging-db.log"
	"$GRAPHQL_ENGINE" serve >"$LOGGING_TEST_LOGFILE_PATH" 2>&1 &
	HGE_PIDS=("${HGE_PIDS[@]}" $!)
	set +x

	wait_for_port 8080
	kill_hge_servers
	# end verbose logging

	# running HGE server again for pytest, the test will use the log generated from the previous run
	# see https://github.com/hasura/graphql-engine-mono/pull/3813 for more information
	run_hge_with_args serve
	wait_for_port 8080
	pytest "${PYTEST_COMMON_ARGS[@]}" \
		--test-startup-db-calls \
		test_startup_db_calls.py

	kill_hge_servers

	unset HASURA_GRAPHQL_ENABLED_LOG_TYPES
	unset HASURA_GRAPHQL_LOG_LEVEL
	unset HASURA_GRAPHQL_ADMIN_SECRET
	unset LOGGING_TEST_LOGFILE_PATH

	;;

read-only-db)
	## read-only DB tests; Hasura should start and run read queries against a read-only DB
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH READ-ONLY DATABASE ########>\n"
	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	export HASURA_GRAPHQL_ENABLED_LOG_TYPES="startup,http-log,webhook-log,websocket-log,query-log"
	export HASURA_GRAPHQL_LOG_LEVEL="debug"
	export HASURA_GRAPHQL_DEV_MODE="false"
	export HASURA_GRAPHQL_ADMIN_INTERNAL_ERRORS="false"

	# setup the database for read-only access
	# 'test_graphql_read_only_source.py' assumes 'HASURA_READONLY_DB_URL' is set
	# Note: setting default_transaction_mode to read-only etc. doesn't work for
	# DDL statements. To replicate read-only access even for DDLs, we need to
	# create a read-only user
	readonly_sql=$(cat <<EOF
CREATE USER hasuraro WITH PASSWORD 'passme';
GRANT CONNECT ON DATABASE pg_source_1 TO hasuraro;
GRANT USAGE ON SCHEMA public TO hasuraro;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO hasuraro;
GRANT SELECT ON ALL TABLES IN SCHEMA pg_catalog TO hasuraro;
GRANT SELECT ON ALL TABLES IN SCHEMA information_schema TO hasuraro;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO hasuraro;
EOF
)
	psql "$HASURA_GRAPHQL_PG_SOURCE_URL_1" -c "$readonly_sql"

	export HASURA_READONLY_DB_URL="postgresql://hasuraro:passme@localhost:5432/pg_source_1"

	run_hge_with_args serve
	wait_for_port 8080

	# and then test graphql queries work
	pytest "${PYTEST_REPORTING_ARGS[@]}" \
		--hge-urls "$HGE_URL" \
		--pg-urls "$HASURA_GRAPHQL_PG_SOURCE_URL_1" \
		--test-read-only-source \
		test_graphql_read_only_source.py

	unset HASURA_GRAPHQL_ENABLED_LOG_TYPES
	kill_hge_servers

	# end read-only DB tests
	;;

remote-schema-https)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH SECURE REMOTE SCHEMA #########################>\n"

	OLD_REMOTE_SCHEMAS_WEBHOOK_DOMAIN="${REMOTE_SCHEMAS_WEBHOOK_DOMAIN}"
	export REMOTE_SCHEMAS_WEBHOOK_DOMAIN="https://localhost:5000"
	init_ssl

	run_hge_with_args serve
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		--tls-ca-cert="$OUTPUT_FOLDER/ssl/ca.pem" --tls-ca-key="$OUTPUT_FOLDER/ssl/ca-key.pem" \
		test_schema_stitching.py::TestRemoteSchemaBasic

	export REMOTE_SCHEMAS_WEBHOOK_DOMAIN="${OLD_REMOTE_SCHEMAS_WEBHOOK_DOMAIN}"
	kill_hge_servers
	;;

webhook-request-context)
	webhook_tests_check_root

	echo -e "\n$(time_elapsed): <########## TEST WEBHOOK RECEIVES REQUEST DATA AS CONTEXT #########################>\n"
	export HASURA_GRAPHQL_AUTH_HOOK="http://localhost:5594/"
	export HASURA_GRAPHQL_AUTH_HOOK_MODE="POST"
	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	run_hge_with_args serve
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		test_webhook_request_context.py

	kill_hge_servers
	;;

post-webhook)
	webhook_tests_check_root

	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET & WEBHOOK (POST) #########################>\n"

	export HASURA_GRAPHQL_AUTH_HOOK="https://localhost:9090/"
	export HASURA_GRAPHQL_AUTH_HOOK_MODE="POST"
	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
	init_ssl

	run_hge_with_args serve
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		--tls-ca-cert="$OUTPUT_FOLDER/ssl/ca.pem" --tls-ca-key="$OUTPUT_FOLDER/ssl/ca-key.pem" \
		-- test_webhook.py

	kill_hge_servers
	;;

get-webhook)
	webhook_tests_check_root

	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET & WEBHOOK (GET) #########################>\n"
	export HASURA_GRAPHQL_AUTH_HOOK="https://localhost:9090/"
	export HASURA_GRAPHQL_AUTH_HOOK_MODE="GET"
	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
	init_ssl

	run_hge_with_args serve
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		--tls-ca-cert="$OUTPUT_FOLDER/ssl/ca.pem" --tls-ca-key="$OUTPUT_FOLDER/ssl/ca-key.pem" \
		-- test_webhook.py

	kill_hge_servers
	;;

insecure-webhook)
	webhook_tests_check_root

	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET & HTTPS INSECURE WEBHOOK (GET) ########>\n"
	export HASURA_GRAPHQL_AUTH_HOOK="https://localhost:9090/"
	export HASURA_GRAPHQL_AUTH_HOOK_MODE="GET"
	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
	init_ssl

	run_hge_with_args serve
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		--tls-ca-cert="$OUTPUT_FOLDER/ssl/ca.pem" --tls-ca-key="$OUTPUT_FOLDER/ssl/ca-key.pem" \
		test_webhook_insecure.py

	kill_hge_servers
	;;

insecure-webhook-with-admin-secret)
	webhook_tests_check_root

	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN_SECRET & HTTPS INSECURE WEBHOOK WITH ADMIN SECRET (POST) ########>\n"
	export HASURA_GRAPHQL_AUTH_HOOK="https://localhost:9090/"
	export HASURA_GRAPHQL_AUTH_HOOK_MODE="POST"
	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
	init_ssl

	run_hge_with_args serve
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		--tls-ca-cert="$OUTPUT_FOLDER/ssl/ca.pem" --tls-ca-key="$OUTPUT_FOLDER/ssl/ca-key.pem" \
		test_webhook_insecure.py

	kill_hge_servers
	;;

apollo-federation)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH APOLLO FEDERATION ########>\n"

	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM"
	export HASURA_GRAPHQL_EXPERIMENTAL_FEATURES="apollo_federation"
	run_hge_with_args serve
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		test_apollo_federation.py

	unset HASURA_GRAPHQL_EXPERIMENTAL_FEATURES
	unset HASURA_GRAPHQL_ADMIN_SECRET

	kill_hge_servers
	;;

allowlist-queries)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ALLOWLIST QUERIES ########> \n"
	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"
	export HASURA_GRAPHQL_ENABLE_ALLOWLIST=true

	run_hge_with_args serve
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		test_allowlist_queries.py

	kill_hge_servers
	;;

developer-api-tests)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH DEVELOPER API ENABLED ########>\n"
	export HASURA_GRAPHQL_ENABLED_APIS="metadata,graphql,developer,config,pgdump"
	export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

	run_hge_with_args serve --enabled-apis "$HASURA_GRAPHQL_ENABLED_APIS"
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		test_dev_endpoints.py

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

	export JWK_SERVER_URL='http://localhost:5001'

	# Start the JWK server.
	# There is a fixture to do this, but when running in this fashion, we need to
	# start the JWK server first so the HGE server can communicate with it.
	python3 jwk_server.py >"$OUTPUT_FOLDER/jwk_server.log" 2>&1 &
	JWKS_PID=$!
	wait_for_port 5001

	echo "Test: Cache-Control with max-age=3"
	export HASURA_GRAPHQL_JWT_SECRET="{\"jwk_url\": \"${JWK_SERVER_URL}/jwk-cache-control?max-age=3\"}"
	run_hge_with_args serve
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		-- 'test_jwk.py::test_cache_control_header_max_age'

	kill_hge_servers
	unset HASURA_GRAPHQL_JWT_SECRET

	echo "Test: Cache-Control with must-revalidate, max-age=3"
	export HASURA_GRAPHQL_JWT_SECRET="{\"jwk_url\": \"${JWK_SERVER_URL}/jwk-cache-control?max-age=3&must-revalidate=true\"}"
	run_hge_with_args serve
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		-- 'test_jwk.py::test_cache_control_header_max_age_must_revalidate'

	kill_hge_servers
	unset HASURA_GRAPHQL_JWT_SECRET

	echo "Test: Cache-Control with must-revalidate"
	export HASURA_GRAPHQL_JWT_SECRET="{\"jwk_url\": \"${JWK_SERVER_URL}/jwk-cache-control?must-revalidate=true\"}"
	run_hge_with_args serve
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		-- 'test_jwk.py::test_cache_control_header_must_revalidate'

	kill_hge_servers
	unset HASURA_GRAPHQL_JWT_SECRET

	echo "Test: Cache-Control with no-cache, public"
	export HASURA_GRAPHQL_JWT_SECRET="{\"jwk_url\": \"${JWK_SERVER_URL}/jwk-cache-control?no-cache=true&public=true\"}"
	run_hge_with_args serve
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		-- 'test_jwk.py::test_cache_control_header_no_cache_public'

	kill_hge_servers
	unset HASURA_GRAPHQL_JWT_SECRET

	echo "Test: Cache-Control with no-store, max-age=3"
	export HASURA_GRAPHQL_JWT_SECRET="{\"jwk_url\": \"${JWK_SERVER_URL}/jwk-cache-control?no-store=true&max-age=3\"}"
	run_hge_with_args serve
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		-- 'test_jwk.py::test_cache_control_header_no_store_max_age'

	kill_hge_servers
	unset HASURA_GRAPHQL_JWT_SECRET

	echo "Test: Expires with three second expiry"
	export HASURA_GRAPHQL_JWT_SECRET="{\"jwk_url\": \"${JWK_SERVER_URL}/jwk-expires?seconds=3\"}"
	run_hge_with_args serve
	wait_for_port 8080

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		-- 'test_jwk.py::test_expires_header'

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

	HASURA_HS_TEST_DB='postgresql://postgres:postgres@localhost:6543/hs_hge_test'

	if ! psql "$HASURA_GRAPHQL_DATABASE_URL" -c "SELECT 1 FROM pg_database WHERE datname = 'hs_hge_test'" | grep -q -F '(1 row)'; then
		psql "$HASURA_GRAPHQL_DATABASE_URL" -c 'CREATE DATABASE hs_hge_test;'
	fi

	pgUserInfo=$(python3 -c '
import os
from urllib.parse import urlparse
uri = urlparse( os.environ["HASURA_GRAPHQL_DATABASE_URL"] )
if uri.password:
    print("password="+uri.password+" user="+uri.username)
else:
    print("user="+uri.username)')

	pgDbInfo=$(psql "$HASURA_GRAPHQL_DATABASE_URL" -c "SELECT concat(' host=',inet_server_addr(),' port=', inet_server_port(),' dbname=',current_database())" | sed -n '3 p')

	# create pgbouncer user
	id pgbouncer || useradd pgbouncer
	cd "$CIRCLECI_FOLDER"
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
admin_users = postgres' >pgbouncer/pgbouncer.ini

	# start pgbouncer
	pgbouncer -u pgbouncer -d pgbouncer/pgbouncer.ini

	cd "$PYTEST_ROOT"
	sleep 2

	# start 1st server
	run_hge_with_args --database-url "$HASURA_HS_TEST_DB" serve
	wait_for_port 8080

	# start 2nd server
	run_hge_with_args --database-url "$HASURA_HS_TEST_DB" serve \
		--server-port 8081
	wait_for_port 8081

	# run test
	pytest "${PYTEST_COMMON_ARGS[@]}" \
		--test-hge-scale-url="http://localhost:8081" \
		test_horizontal_scale.py

	# Shutdown pgbouncer
	psql "postgresql://postgres:postgres@localhost:6543/pgbouncer" -c "SHUTDOWN;" || true

	cd "$CIRCLECI_FOLDER"

	# start pgbouncer again
	pgbouncer -u pgbouncer -d pgbouncer/pgbouncer.ini

	cd "$PYTEST_ROOT"

	# sleep for 20 seconds
	sleep 20

	# run test
	pytest "${PYTEST_COMMON_ARGS[@]}" \
		--test-hge-scale-url="http://localhost:8081" \
		test_horizontal_scale.py

	# Shutdown pgbouncer
	psql "postgresql://postgres:postgres@localhost:6543/pgbouncer" -c "SHUTDOWN;" || true

	kill_hge_servers

	psql "$HASURA_GRAPHQL_DATABASE_URL" -c "drop database hs_hge_test;"
	sleep 4
	unset HASURA_HS_TEST_DB

	# end horizontal scale test
	;;
#
# ###########################################
# the following backend-* tests are excluded from `server-test-names.txt`
# and are run via their respective `test_oss_server_*` jobs
#
# [Specifying Pytests with -k flag]
# tests are run with the -k flag to filter on common and
# backend-specific test classes using keyword expressions.
#
# this reduces the number of unrelated tests skipped, which
# avoids an increasingly negative impact on our test run
# time as we add more backends and tests.
#
# https://docs..org/en/6.2.x/usage.html#specifying-tests-selecting-tests
# https://github.com/hasura/graphql-engine/blob/master/server/py-tests/README.md#running-bigquery-tests
#
backend-mssql)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH SQL SERVER BACKEND ###########################################>\n"

	export HASURA_GRAPHQL_EXPERIMENTAL_FEATURES=naming_convention
	run_hge_with_args serve
	wait_for_port 8080

	source_data_sources_utils
	add_mssql_source 8080 "$HASURA_GRAPHQL_MSSQL_SOURCE_URL"

	# See note [Specifying Pytests with -k flag]
	pytest "${PYTEST_COMMON_ARGS[@]}" \
		--backend=mssql \
		-k "MSSQL"

	# start inherited roles test
	echo -e "\n$(time_elapsed): <########## TEST INHERITED-ROLES WITH SQL SERVER BACKEND ###########################################>\n"

	pytest "${PYTEST_COMMON_ARGS[@]}" \
		--backend=mssql \
		-k TestGraphQLInheritedRolesMSSQL

	# end inherited roles test

	# start naming conventions test (failure for other than postgres backend)
	echo -e "\n$(time_elapsed): <########## TEST NAMING CONVENTIONS WITH SQL SERVER BACKEND ###########################################>\n"
	pytest "${PYTEST_COMMON_ARGS[@]}" \
		--backend mssql \
		-k TestNamingConventionsFailure
	export HASURA_GRAPHQL_EXPERIMENTAL_FEATURES=$DEFAULT_HASURA_EXPERIMENTAL_FEATURES
	# end naming conventions test

	kill_hge_servers
	;;
backend-citus)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH CITUS BACKEND ###########################################>\n"

	run_hge_with_args serve
	wait_for_port 8080

	source_data_sources_utils
	add_citus_source 8080 "$HASURA_GRAPHQL_CITUS_SOURCE_URL"

	# See note [Specifying Pytests with -k flag]
	pytest "${PYTEST_COMMON_ARGS[@]}" \
		--backend=citus \
		-k "Citus"

	kill_hge_servers
	;;
backend-bigquery)
	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH BIGQUERY BACKEND ###########################################>\n"

	# shellcheck source=../scripts/bigquery.sh
	source "$CIRCLECI_FOLDER/../scripts/bigquery.sh" && verify_bigquery_pytest_env

	run_hge_with_args serve
	wait_for_port 8080

	source_data_sources_utils
	add_bigquery_source 8080

	# See note [Specifying Pytests with -k flag]
	pytest "${PYTEST_COMMON_ARGS[@]}" \
		--backend=bigquery \
		-k "Bigquery"

	kill_hge_servers
	;;
esac

echo "Finished running tests on node $CIRCLE_NODE_INDEX of $CIRCLE_NODE_TOTAL"
echo -e "\n$(time_elapsed): <########## DONE ########>\n"
