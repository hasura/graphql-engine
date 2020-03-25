set -euo pipefail


# parameters

DB_NAME='hasura-tests'
POSTGRES="postgres://localhost:4200/$DB_NAME"


# cleanup

WH_PID=""

function finish {
    function terminate() {
        pid="$1"
        if [ -n "$pid" ] ; then
            kill $pid || true
            wait $pid || true
        fi
    }
    ps | grep graphql | tr -s ' ' | cut -d ' ' -f 2 | xargs kill 2> /dev/null || true
    terminate "$WH_PID"
}
trap finish EXIT


# helpers

wait_for_port() {
    local service=$1
    local port=$2
    echo -n "Waiting for $service on port $port..."
    for _ in $(seq 1 60);
    do
      nc -z localhost $port && echo " ready" && return
      echo -n .
      sleep 0.25
    done
    echo " timeout, aborting" && exit 1
}


# create db

cd $(dirname $(realpath "$0"))/server
dropdb   -p 4200 ${DB_NAME} --if-exists
createdb -p 4200 ${DB_NAME}


# get python env

source .python-venv/bin/activate
pip3 install -r tests-py/requirements.txt > /dev/null


# build and run graphql-engine

export HASURA_GRAPHQL_ADMIN_SECRET="s3cr3t"
export EVENT_WEBHOOK_HEADER="MyEnvValue"
export WEBHOOK_FROM_ENV="http://localhost:5592/"
export HASURA_GRAPHQL_AUTH_HOOK="https://localhost:9090/"

cabal new-build
cabal new-run -- exe:graphql-engine \
  --database-url=${POSTGRES}        \
  serve                             \
  --stringify-numeric-types         \
  2>&1 > tests-py/hasura.log &
wait_for_port 'graphql-engine' 8080


# run webhook

cd tests-py

if [ ! -f ssl/webhook-req.cnf ] ; then
    echo "Warning: no SSL configuration found, creating new"
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

    mkdir -p ssl
    pushd ssl
    echo "$CNF_TEMPLATE" > webhook-req.cnf

    openssl genrsa -out ca-key.pem 2048
    openssl req -x509 -new -nodes -key ca-key.pem -days 10 -out ca.pem -subj "/CN=webhook-ca"
    openssl genrsa -out webhook-key.pem 2048
    openssl req -new -key webhook-key.pem -out webhook.csr -subj "/CN=hge-webhook" -config webhook-req.cnf
    openssl x509 -req -in webhook.csr -CA ca.pem -CAkey ca-key.pem -CAcreateserial -out webhook.pem -days 10 -extensions v3_req -extfile webhook-req.cnf

    sudo cp ca.pem /etc/ssl/certs/webhook.crt
    # arch / manjaro specific
    sudo trust extract-compat # update-ca-certificates
    popd
fi

python3 webhook.py 9090 ssl/webhook-key.pem ssl/webhook.pem > webhook.log 2>&1 &
WH_PID=$!
wait_for_port 'webhook' 9090


# running tests

pytest \
    --hge-key "s3cr3t" \
    --hge-urls http://localhost:8080/ \
    --hge-webhook https://localhost:9090/ \
    --pg-urls ${POSTGRES} \
    "$@" || true
