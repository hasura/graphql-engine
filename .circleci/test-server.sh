#!/usr/bin/env bash
set -euo pipefail

### Functions

stop_services() {
   kill -INT $PID
   kill $WH_PID
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

if [ -z "${HASURA_GRAPHQL_DATABASE_URL:-}" ] ; then
	echo "Env var HASURA_GRAPHQL_DATABASE_URL is not set"
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

echo -e "\nINFO: GraphQL Executable : $GRAPHQL_ENGINE"
echo -e "INFO: Logs Folder        : $OUTPUT_FOLDER\n"

pip3 install -r requirements.txt

mkdir -p "$OUTPUT_FOLDER"

export EVENT_WEBHOOK_HEADER="MyEnvValue"
export HGE_URL="http://localhost:8080"
export WEBHOOK_FROM_ENV="http://127.0.0.1:5592"

PID=""
WH_PID=""
trap stop_services ERR
trap stop_services INT

echo -e "\n<########## TEST GRAPHQL-ENGINE WITHOUT ADMIN SECRET ###########################################>\n"

"$GRAPHQL_ENGINE" serve > "$OUTPUT_FOLDER/graphql-engine.log" & PID=$!

wait_for_port 8080

pytest -vv --hge-url="$HGE_URL" --pg-url="$HASURA_GRAPHQL_DATABASE_URL"

kill -INT $PID
sleep 4
mv graphql-engine.tix graphql-engine-combined.tix || true

##########
echo -e "\n<########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET #####################################>\n"

export HASURA_GRAPHQL_ADMIN_SECRET="HGE$RANDOM$RANDOM"

"$GRAPHQL_ENGINE" serve >> "$OUTPUT_FOLDER/graphql-engine.log" & PID=$!

wait_for_port 8080

pytest -vv --hge-url="$HGE_URL" --pg-url="$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET"

kill -INT $PID
sleep 4
combine_hpc_reports

##########
echo -e "\n<########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET AND JWT #####################################>\n"

init_jwt

export HASURA_GRAPHQL_JWT_SECRET="$(jq -n --arg key "$(cat $OUTPUT_FOLDER/ssl/jwt_public.key)" '{ type: "RS512", key: $key }')"

"$GRAPHQL_ENGINE" serve >> "$OUTPUT_FOLDER/graphql-engine.log" & PID=$!

pytest -vv --hge-url="$HGE_URL" --pg-url="$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-jwt-key-file="$OUTPUT_FOLDER/ssl/jwt_private.key"

kill -INT $PID
sleep 4
combine_hpc_reports

unset HASURA_GRAPHQL_JWT_SECRET

##########

if [ $EUID != 0 ] ; then
	echo -e "SKIPPING webhook based tests, as \nroot permission is required for running webhook tests (inorder to trust certificate authority)."
	RUN_WEBHOOK_TESTS=false
fi

if [ "$RUN_WEBHOOK_TESTS" == "true" ] ; then

	echo -e "\n<########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET & WEBHOOK (GET) #########################>\n"

	export HASURA_GRAPHQL_AUTH_HOOK="https://localhost:9090/"
	init_ssl

	"$GRAPHQL_ENGINE" serve >> "$OUTPUT_FOLDER/graphql-engine.log" 2>&1 & PID=$!

	python3 webhook.py 9090 "$OUTPUT_FOLDER/ssl/webhook-key.pem" "$OUTPUT_FOLDER/ssl/webhook.pem" > "$OUTPUT_FOLDER/webhook.log" 2>&1  & WH_PID=$!

	wait_for_port 8080

	wait_for_port 9090

	pytest -vv --hge-url="$HGE_URL" --pg-url="$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-webhook="$HASURA_GRAPHQL_AUTH_HOOK"

	kill -INT $PID
	sleep 4
	combine_hpc_reports

  echo -e "\n<########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET & WEBHOOK (POST) #########################>\n"
  export HASURA_GRAPHQL_AUTH_HOOK_MODE="POST"

	"$GRAPHQL_ENGINE" serve >> "$OUTPUT_FOLDER/graphql-engine.log" 2>&1 & PID=$!

  wait_for_port 8080

	pytest -vv --hge-url="$HGE_URL" --pg-url="$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-webhook="$HASURA_GRAPHQL_AUTH_HOOK"

	rm /etc/ssl/certs/webhook.crt
	update-ca-certificates

	kill -INT $PID
	sleep 4
	combine_hpc_reports

	echo -e "\n<########## TEST GRAPHQL-ENGINE WITH ADMIN SECRET & HTTPS INSECURE WEBHOOK (GET) ########>\n"
  export HASURA_GRAPHQL_AUTH_HOOK_MODE="GET"

	"$GRAPHQL_ENGINE" serve >> "$OUTPUT_FOLDER/graphql-engine.log" 2>&1 & PID=$!

	wait_for_port 8080

	pytest -vv --hge-url="$HGE_URL" --pg-url="$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-webhook="$HASURA_GRAPHQL_AUTH_HOOK" --test-webhook-insecure test_webhook_insecure.py

	kill -INT $PID
	sleep 4
	combine_hpc_reports

	echo -e "\n<########## TEST GRAPHQL-ENGINE WITH ADMIN_SECRET & HTTPS INSECURE WEBHOOK (POST) ########>\n"
  export HASURA_GRAPHQL_AUTH_HOOK_MODE="POST"

	"$GRAPHQL_ENGINE" serve >> "$OUTPUT_FOLDER/graphql-engine.log" 2>&1 & PID=$!

	wait_for_port 8080

	pytest -vv --hge-url="$HGE_URL" --pg-url="$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ADMIN_SECRET" --hge-webhook="$HASURA_GRAPHQL_AUTH_HOOK" --test-webhook-insecure test_webhook_insecure.py

	kill -INT $PID
	sleep 4
	combine_hpc_reports

	kill $WH_PID
fi

mv graphql-engine-combined.tix "$OUTPUT_FOLDER/graphql-engine.tix" || true
