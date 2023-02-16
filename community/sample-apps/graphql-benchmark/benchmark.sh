#!/bin/sh

docker compose -f docker-compose.hasura.yml up -d --build

echo "Running Hasura Benchmark"

sleep 5

docker run --net=host -v "$PWD":/app/tmp -it \
 graphql-bench-local query \
 --config="./tmp/config.query.hasura.yaml" \
 --outfile="./tmp/report.hasura.json"

docker compose -f docker-compose.hasura.yml down

echo "Hasura Benchmark done"

docker compose -f docker-compose.node.yml up -d --build

echo "Running Nodejs Benchmark"

sleep 5

docker run --net=host -v "$PWD":/app/tmp -it \
 graphql-bench-local query \
 --config="./tmp/config.query.node.yaml" \
 --outfile="./tmp/report.nodejs.json"

docker compose -f docker-compose.node.yml down

echo "Node.js Benchmark done"
