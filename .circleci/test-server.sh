#!/usr/bin/env bash
set -euo pipefail
PYTEST_ROOT="${BASH_SOURCE[0]%/*}/../server/tests-py"

cd $PYTEST_ROOT

pip3 install -r requirements.txt

wait_for_port() {
    local PORT=$1
    echo "waiting for $PORT"
    for i in `seq 1 60`;
    do
      nc -z localhost $PORT && echo "port $PORT is ready" && return
      echo -n .
      sleep 1
    done
    echo "Failed waiting for $PORT" && exit 1
}

init_ssl() {
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
}

mkdir -p /build/_server_test_output/$PG_VERSION

export HASURA_GRAPHQL_DATABASE_URL="postgres://gql_test:@localhost:5432/gql_test"
export EVENT_WEBHOOK_HEADER="MyEnvValue"
export HGE_URL="http://localhost:8080"

##########
echo "Test HGE without access keys"

/build/_server_output/graphql-engine serve > /build/_server_test_output/$PG_VERSION/server.log & PID=$!

wait_for_port 8080

pytest -vv --hge-url="$HGE_URL" --pg-url="$HASURA_GRAPHQL_DATABASE_URL"

kill $PID


########## 
echo "<------------- Test graphql-engine with access key ---------------------------------->"

export HASURA_GRAPHQL_ACCESS_KEY="HGE$RANDOM$RANDOM"

/build/_server_output/graphql-engine serve >> /build/_server_test_output/$PG_VERSION/server.log & PID=$!

wait_for_port 8080

pytest -vv --hge-url="$HGE_URL" --pg-url="$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ACCESS_KEY"

kill $PID


########## 
echo "<------------- Test graphql-engine with access key and webhook ---------------------->"

export HASURA_GRAPHQL_AUTH_HOOK="https://localhost:9090/"
init_ssl

/build/_server_output/graphql-engine serve >> /build/_server_test_output/$PG_VERSION/server.log 2>&1 & PID=$!

python3 webhook.py > /build/_server_test_output/$PG_VERSION/server.log 2>&1  & WH_PID=$!

wait_for_port 8080

wait_for_port 9090

pytest -vv --hge-url="$HGE_URL" --pg-url="$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ACCESS_KEY" --hge-webhook="$HASURA_GRAPHQL_AUTH_HOOK"

rm /etc/ssl/certs/webhook.crt
update-ca-certificates

kill $PID

###########
echo "<------------- Test graphql-engine with access key and an HTTPS inseure webhook ----->"

/build/_server_output/graphql-engine serve >> /build/_server_test_output/$PG_VERSION/server.log 2>&1 & PID=$!

pytest -vv --hge-url="$HGE_URL" --pg-url="$HASURA_GRAPHQL_DATABASE_URL" --hge-key="$HASURA_GRAPHQL_ACCESS_KEY" --hge-webhook="$HASURA_GRAPHQL_AUTH_HOOK" --test-webhook-insecure test_webhook_insecure.py

kill $PID

kill $WH_PID

