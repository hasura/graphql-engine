# GraphQL Benchmarking

Uses the [Chinook sample database](https://github.com/lerocha/chinook-database). Tested on macOS Ventura 13.1.

## Instructions

1. Checkout [https://github.com/hasura/graphql-bench](https://github.com/hasura/graphql-bench) and build the docker container with `make build_local_docker_image`

1. Run `docker compose -f docker-compose.hasura.yml up -d` to bootstrap Postgres with Chinook.

1. After a few minutes check the Hasura docker logs to see if Hasura is running, which implies the database is now bootstrapped. Run `docker compose -f docker-compose.hasura.yml down`

1. Run the benchmarks `sh benchmark.sh`

1. Open the results on the GraphQL bench website [https://hasura.github.io/graphql-bench/app/web-app/](https://hasura.github.io/graphql-bench/app/web-app/)
