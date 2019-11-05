## Benchmarking Hasura GraphQL Engine ##

The script `hge_wrk_bench.py` helps in benchmarking the given version of Hasura GraphQL Engine using a set of GraphQL queries. The results are stored (into the *results GraphQL engine*) along with details like the version of GraphQL engine against which the benchmark is run, the version of Postgres database etc. The stored results can help in comparing benchmarks of different versions of GraphQL engine.

### Setup ###

The setup includes two Postgres databases with [sportsdb](https://www.thesportsdb.com/) schema and data, and two GraphQL engines running on the Postgres databases. Then one of the GraphQL engines is added as a remote schema to another GraphQL engine.

The data will be same in both the databases. The tables reside in different database schema in-order to avoid GraphQL schema conflicts.

The methods in script `sportsdb_setup.py` helps in setting up the databases, starting the Hasura GraphQL engines, and setting up relationships. This script can either take urls of already running Postgres databases as input, or it can start the databases as Docker instances. The GraphQL engines can be run either with `stack exec` or as Docker containers.

### Run benchmark ###
- Install dependencies for the Python script in a virtual environment
```sh
$ python3 -m venv venv
$ source venv/bin/activate
$ pip3 install -r requirements.txt
```
- To run benchmarks, do
```sh
$ python3 hge_wrk_bench.py
```
This script uses [wrk](https://github.com/wg/wrk) to benchmark Hasura GraphQL Engine against a list of queries defined in `queries.graphql`. The results are then stored through a results Hasura GraphQL Engine.

### Arguments ###
- For the list of arguments supported, do
```sh
$ python3 hge_wrk_bench.py --help
```

#### Postgres ####
  - In order to use already runnning Postgres databases, use argument `--pg-urls PG_URL,REMOTE_PG_URL`, or environmental variable `export HASURA_BENCH_PG_URLS=PG_URL,PG_URL_REMOTE`
  - Set the docker image using argument `--pg-docker-image DOCKER_IMAGE`, or environmental variable `HASURA_BENCH_PG_DOCKER_IMAGE`

#### GraphQL Engine ####
  - Inorder to run as a docker container, use argument `--hge-docker-image DOCKER_IMAGE`, or environmental variable `HASURA_BENCH_HGE_DOCKER_IMAGE`
  - To skip stack build, use argument `--skip-stack-build`

#### wrk ####
  - Number of open connections can be set using argument `--connections CONNECTIONS`, or environmental variable `HASURA_BENCH_CONNECTIONS`
  - Duration of tests can be controlled using argument `--duration DURATION`, or environmental variable `HASURA_BENCH_CONNECTIONS`
  - If plots should not have to be shown at the end of benchmarks, use argument `--skip-plots`
  #TODO About where should the results be stored

### Work directory ###
- The files used by Postgres docker containers, logs of Hasura GraphQL engines run with stack exec, and other stuff are stored in the work directory.
- Storing data volumes of Postgres docker containers in the work directory helps in avoiding database setup time for benchmarks after the first time setup.
- The logs of Hasura GraphQL engines (when they are run using stack exec) are stored in files *hge.log* and *remote\_hge.log*

### Default settings ###
- Postgres databases will be run as docker containers
- Hasura GraphQL Engines by default will be run using `stack exec`. `stack build` will be done before that.
- With wrk
  - Number of threads used by *wrk* will be number of CPUs
  - Number of connections = 50
  - Test duration = 5 minutes (300 sec)
