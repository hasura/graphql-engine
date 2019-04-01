#!/usr/bin/env bash
set -euo pipefail

### Functions

stop_services() {
   kill -INT $PID || true
   kill $WH_PID || true
   kill -INT $WHC_PID || true
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
    for _ in $(seq 1 240);
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

combine_hpc_reports() {
	(stack --allow-different-user exec -- hpc combine graphql-engine.tix graphql-engine-combined.tix --union > graphql-engine-combined.tix2 && mv graphql-engine-combined.tix2 graphql-engine-combined.tix ) || true
	rm graphql-engine.tix || true
}

kill_hge_and_combine_hpc_reports() {
	kill -INT $PID
	wait $PID || true
	combine_hpc_reports
}

start_gql_servers() {
	"$GRAPHQL_ENGINE" --database-url "$HASURA_GRAPHQL_DATABASE_URL" serve $@ > "$OUTPUT_FOLDER/graphql-engine.log" & PID=$!
	if [ -n ${HASURA_GRAPHQL_DATABASE_URL_2:-} ] ; then
		"$GRAPHQL_ENGINE" --database-url "$HASURA_GRAPHQL_DATABASE_URL_2" serve --server-port 8081 $@ > "$OUTPUT_FOLDER/graphql-engine-2.log" & PID="$PID $!"
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

if ! stack --allow-different-user exec which hpc ; then
	echo "hpc not found; Install it with 'stack install hpc'"
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

cd $PYTEST_ROOT

if ! stack --allow-different-user exec -- which graphql-engine > /dev/null && [ -z "${GRAPHQL_ENGINE:-}" ] ; then
	echo "Do 'stack build' before tests, or export the location of executable in the GRAPHQL_ENGINE envirnoment variable"
	exit 1
fi
GRAPHQL_ENGINE=${GRAPHQL_ENGINE:-"$(stack --allow-different-user exec -- which graphql-engine)"}
if ! [ -x "$GRAPHQL_ENGINE" ] ; then
	echo "$GRAPHQL_ENGINE is not present or is not an executable"
	exit 1
fi
RUN_WEBHOOK_TESTS=true

for port in 8080 8081 9876 5592
do
	fail_if_port_busy $port
done

echo -e "\nINFO: GraphQL Executable : $GRAPHQL_ENGINE"
echo -e "INFO: Logs Folder        : $OUTPUT_FOLDER\n"

pip3 install -r requirements.txt

mkdir -p "$OUTPUT_FOLDER"

export EVENT_WEBHOOK_HEADER="MyEnvValue"
export HGE_URL="http://localhost:8080"
export HGE_URL_2=""
if [ -n ${HASURA_GRAPHQL_DATABASE_URL_2:-} ] ; then
	HGE_URL_2="http://localhost:8081"
fi
export WEBHOOK_FROM_ENV="http://127.0.0.1:5592"
export HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES=true

PID=""
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

echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITHOUT ADMIN SECRET ###########################################>\n"
rm graphql-engine.tix || true

start_gql_servers

run_pytest_parallel 

kill -INT $PID
sleep 4
mv graphql-engine.tix graphql-engine-combined.tix || true


##########
echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET #####################################>\n"

export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

start_gql_servers

run_pytest_parallel --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" 

kill_hge_and_combine_hpc_reports


##########
echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT #####################################>\n"

init_jwt

export HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat $OUTPUT_FOLDER/ssl/jwt_public.key)" '{ type: "RS512", key: $key }')"

start_gql_servers

run_pytest_parallel --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET" 

kill_hge_and_combine_hpc_reports

unset HASURA_GRAPHQL_JWT_SECRET

echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT (in stringified mode) #####################################>\n"

export HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat $OUTPUT_FOLDER/ssl/jwt_public.key)" '{ type: "RS512", key: $key , claims_format: "stringified_json"}')"

"$GRAPHQL_ENGINE" serve >> "$OUTPUT_FOLDER/graphql-engine.log" & PID=$!

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key" --hge-jwt-conf="$HASURA_GRAPHQL_JWT_SECRET" test_jwt.py

kill_hge_and_combine_hpc_reports

unset HASURA_GRAPHQL_JWT_SECRET

# test with CORS modes

echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH CORS DOMAINS ########>\n"
export HASURA_GRAPHQL_CORS_DOMAIN="http://*.localhost, http://localhost:3000, https://*.foo.bar.com"

"$GRAPHQL_ENGINE" serve >> "$OUTPUT_FOLDER/graphql-engine.log" 2>&1 & PID=$!

wait_for_port 8080

pytest -n  1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-cors test_cors.py

kill_hge_and_combine_hpc_reports

unset HASURA_GRAPHQL_CORS_DOMAIN

# test websocket transport with initial cookie header

echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH COOKIE IN WEBSOCKET INIT ########>\n"
export HASURA_GRAPHQL_AUTH_HOOK="http://localhost:9876/auth"
export HASURA_GRAPHQL_AUTH_HOOK_MODE="POST"

python3 test_cookie_webhook.py > "$OUTPUT_FOLDER/cookie_webhook.log" 2>&1  & WHC_PID=$!

wait_for_port 9876

"$GRAPHQL_ENGINE" serve >> "$OUTPUT_FOLDER/graphql-engine.log" 2>&1 & PID=$!

wait_for_port 8080

echo "$(time_elapsed): testcase 1: read cookie, cors enabled"
pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-ws-init-cookie=read test_websocket_init_cookie.py

kill -INT $PID
sleep 1

echo "$(time_elapsed): testcase 2: no read cookie, cors disabled"
"$GRAPHQL_ENGINE" serve --disable-cors >> "$OUTPUT_FOLDER/graphql-engine.log" 2>&1 & PID=$!

wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-ws-init-cookie=noread test_websocket_init_cookie.py

kill -INT $PID
sleep 1

echo "$(time_elapsed): testcase 3: read cookie, cors disabled and ws-read-cookie"
export HASURA_GRAPHQL_WS_READ_COOKIE="true"
"$GRAPHQL_ENGINE" serve --disable-cors >> "$OUTPUT_FOLDER/graphql-engine.log" 2>&1 & PID=$!

wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-ws-init-cookie=read test_websocket_init_cookie.py

kill -INT $PID
kill -INT $WHC_PID
unset HASURA_GRAPHQL_WS_READ_COOKIE
unset HASURA_GRAPHQL_AUTH_HOOK
unset HASURA_GRAPHQL_AUTH_HOOK_MODE
sleep 4
combine_hpc_reports


echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH GRAPHQL DISABLED ########>\n"

export HASURA_GRAPHQL_ENABLED_APIS="metadata"

"$GRAPHQL_ENGINE" serve >> "$OUTPUT_FOLDER/graphql-engine.log" 2>&1 & PID=$!

wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-graphql-disabled test_apis_disabled.py

kill_hge_and_combine_hpc_reports

unset HASURA_GRAPHQL_ENABLED_APIS

"$GRAPHQL_ENGINE" serve --enabled-apis metadata >> "$OUTPUT_FOLDER/graphql-engine.log" 2>&1 & PID=$!

wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-graphql-disabled test_apis_disabled.py

kill_hge_and_combine_hpc_reports

echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH METADATA DISABLED ########>\n"

export HASURA_GRAPHQL_ENABLED_APIS="graphql"

"$GRAPHQL_ENGINE" serve >> "$OUTPUT_FOLDER/graphql-engine.log" 2>&1 & PID=$!

wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-metadata-disabled test_apis_disabled.py

kill_hge_and_combine_hpc_reports
unset HASURA_GRAPHQL_ENABLED_APIS

"$GRAPHQL_ENGINE" serve --enabled-apis graphql >> "$OUTPUT_FOLDER/graphql-engine.log" 2>&1 & PID=$!

wait_for_port 8080

pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --test-metadata-disabled test_apis_disabled.py

kill_hge_and_combine_hpc_reports


# webhook tests

if [ $EUID != 0 ] ; then
	echo -e "SKIPPING webhook based tests, as \nroot permission is required for running webhook tests (inorder to trust certificate authority)."
	RUN_WEBHOOK_TESTS=false
fi

if [ "$RUN_WEBHOOK_TESTS" == "true" ] ; then

	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET & WEBHOOK (GET) #########################>\n"

	export HASURA_GRAPHQL_AUTH_HOOK="https://localhost:9090/"
	init_ssl

	start_gql_servers

	python3 webhook.py 9090 "$OUTPUT_FOLDER/ssl/webhook-key.pem" "$OUTPUT_FOLDER/ssl/webhook.pem" > "$OUTPUT_FOLDER/webhook.log" 2>&1  & WH_PID=$!

	wait_for_port 9090

	run_pytest_parallel --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-webhook="$HASURA_GRAPHQL_AUTH_HOOK" 

	kill_hge_and_combine_hpc_reports

	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET & WEBHOOK (POST) #########################>\n"
	export HASURA_GRAPHQL_AUTH_HOOK_MODE="POST"

	start_gql_servers

	run_pytest_parallel --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-webhook="$HASURA_GRAPHQL_AUTH_HOOK" 

	rm /etc/ssl/certs/webhook.crt
	update-ca-certificates

	kill_hge_and_combine_hpc_reports

	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET & HTTPS INSECURE WEBHOOK (GET) ########>\n"
  export HASURA_GRAPHQL_AUTH_HOOK_MODE="GET"

	"$GRAPHQL_ENGINE" serve >> "$OUTPUT_FOLDER/graphql-engine.log" 2>&1 & PID=$!

	wait_for_port 8080

	pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-webhook="$HASURA_GRAPHQL_AUTH_HOOK" --test-webhook-insecure test_webhook_insecure.py

	kill_hge_and_combine_hpc_reports

	echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH ADMIN_SECRET & HTTPS INSECURE WEBHOOK (POST) ########>\n"
  export HASURA_GRAPHQL_AUTH_HOOK_MODE="POST"

	"$GRAPHQL_ENGINE" serve >> "$OUTPUT_FOLDER/graphql-engine.log" 2>&1 & PID=$!

	wait_for_port 8080

	pytest -n 1 -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-webhook="$HASURA_GRAPHQL_AUTH_HOOK" --test-webhook-insecure test_webhook_insecure.py

	kill_hge_and_combine_hpc_reports

	kill $WH_PID

fi

# horizontal scale test
unset HASURA_GRAPHQL_AUTH_HOOK
unset HASURA_GRAPHQL_AUTH_HOOK_MODE
unset HASURA_GRAPHQL_ADMIN_SECRET

echo -e "\n$(time_elapsed): <########## TEST GRAPHQL-ENGINE WITH HORIZONTAL SCALING ########>\n"

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
"$GRAPHQL_ENGINE" --database-url "$HASURA_HS_TEST_DB" serve >> "$OUTPUT_FOLDER/graphql-engine.log" 2>&1 & PID=$!
wait_for_port 8080

# start 2nd server
"$GRAPHQL_ENGINE" --database-url "$HASURA_HS_TEST_DB" serve \
                  --server-port 8081 \
                  >> "$OUTPUT_FOLDER/hs-graphql-engine.log" 2>&1 & HS_PID=$!
wait_for_port 8081

# run test
pytest -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --test-hge-scale-url="http://localhost:8081" test_horizontal_scale.py

# Shutdown pgbouncer
psql "postgres://postgres:postgres@localhost:6543/pgbouncer" -c "SHUTDOWN;" || true

cd $CIRCLECI_FOLDER

# start pgbouncer again
pgbouncer -u pgbouncer -d pgbouncer/pgbouncer.ini

cd $PYTEST_ROOT

# sleep for 30 seconds
sleep 20

# run test
pytest -vv --hge-urls "$HGE_URL" --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --test-hge-scale-url="http://localhost:8081" test_horizontal_scale.py

# Shutdown pgbouncer
psql "postgres://postgres:postgres@localhost:6543/pgbouncer" -c "SHUTDOWN;" || true

kill $PID
kill $HS_PID
psql "$HASURA_GRAPHQL_DATABASE_URL" -c "drop database hs_hge_test;"
sleep 4
combine_hpc_reports
unset HASURA_HS_TEST_DB


# end horizontal scale test

mv graphql-engine-combined.tix "$OUTPUT_FOLDER/graphql-engine.tix" || true
