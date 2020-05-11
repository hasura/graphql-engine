## Benchmarking Hasura GraphQL Engine ##

The script `hge_wrk_bench.py` helps in benchmarking the given version of Hasura
GraphQL Engine using a set of GraphQL queries. The results are stored (into the
*results GraphQL engine*) along with details like the version of GraphQL engine
against which the benchmark is run, the version of Postgres database etc. The
stored results can help in comparing benchmarks of different versions of
GraphQL engine.

### Setup ###

The setup includes two Postgres databases with
[sportsdb](https://www.thesportsdb.com/) schema and data, and two GraphQL
engines running on the Postgres databases. Then one of the GraphQL engines is
added as a remote schema to another GraphQL engine.

The data will be same in both the databases. The tables reside in different
database schema in-order to avoid GraphQL schema conflicts.

The methods in script `sportsdb_setup.py` helps in setting up the databases,
starting the Hasura GraphQL engines, and setting up relationships. This script
can either take urls of already running Postgres databases as input, or it can
start the databases as Docker instances. The GraphQL engines can be run either
with `cabal run` or as Docker containers.

### Run benchmark ###
- Install Python 3.7.6 using pyenv
```sh
$ pyenv install 3.7.6
```
- Install dependencies for the Python script in a virtual environment.
```sh
$ python3 -m venv venv
$ source venv/bin/activate
$ pip3 install -r requirements.txt
```
- To run benchmarks, do
```sh
$ python3 hge_wrk_bench.py
```
This script uses [wrk](https://github.com/wg/wrk) to benchmark Hasura GraphQL
Engine against a list of queries defined in `queries.graphql`. The results are
then stored through a results Hasura GraphQL Engine.

You can configure the build and runtime parameters for the graphql-engine's
under test by modifying your local `cabal.project.local` file.

### Interpreting the plots

For each query under test we first run `wrk` to try to determine the maximum
throughput we can sustain for that query. This result is plotted under the `max
throughput` graph. This can be considered the point after which graphql-engine
will start to fall over.

Then for each query we measure latency under several different loads (but
making sure not to approach max throughput) using `wrk2` which measures latency
in a principled way. Latency can be viewed as a continuous histogram or as a
violin plot that also plots each latency sample. The latter provides the most
visual information and can be useful for observing clustering or other
patterns, or validating the benchmark run.

### Cleaning up test runs

Data will be stored locally in the work directory (`test_output` by default).
This entire directory can be deleted safely. 

If you are using the default results graphql-engine and want to just remove old
benchmark runs but avoid rebuilding the sportsdb data, you can do:

```
$ sudo rm -r test_output/{benchmark_runs,sportsdb_data}
```

### Arguments ###
- For the list of arguments supported, do
```sh
$ python3 hge_wrk_bench.py --help
```

#### Postgres ####
  - In order to use already runnning Postgres databases, use argument `--pg-urls PG_URL,REMOTE_PG_URL`, or environmental variable `export HASURA_BENCH_PG_URLS=PG_URL,REMOTE_PG_URL`
  - Set the docker image using argument `--pg-docker-image DOCKER_IMAGE`, or environmental variable `HASURA_BENCH_PG_DOCKER_IMAGE`

#### GraphQL Engine ####
  - Inorder to run as a docker container, use argument `--hge-docker-image DOCKER_IMAGE`, or environmental variable `HASURA_BENCH_HGE_DOCKER_IMAGE`
  - To skip stack build, use argument `--skip-stack-build`

#### wrk ####
  - Number of open connections can be set using argument `--connections CONNECTIONS`, or environmental variable `HASURA_BENCH_CONNECTIONS`
  - Duration of tests can be controlled using argument `--duration DURATION`, or environmental variable `HASURA_BENCH_CONNECTIONS`
  - If plots should not have to be shown at the end of benchmarks, use argument `--skip-plots`
  - The Hasura GraphQL Engine to which resuls should be pushed can be specified using argument 
    `--results-hge-url HGE_URL`, or environmental variable `HASURA_BENCH_RESULTS_HGE_URL`. By 
    default the launched (non-"remote") graphql-engine will be used, and its data stored in 
    `test_output/sportsdb_data`. The admin secret for this GraphQL engine can be specified 
    using environmental variable `HASURA_BENCH_RESULTS_HGE_ADMIN_SECRET`.

### Work directory ###
- The files used by Postgres docker containers, logs of Hasura GraphQL engines run with `cabal run`, and other stuff are stored in the work directory.
- Storing data volumes of Postgres docker containers in the work directory (`test_output` by default) helps in avoiding database setup time for benchmarks after the first time setup.
- The logs of Hasura GraphQL engines (when they are run using `cabal run`) are stored in files *hge.log* and *remote\_hge.log*

### Default settings ###
- Postgres databases will be run as docker containers
- Hasura GraphQL Engines by default will be run using `cabal run`
- With wrk
  - Number of threads used by *wrk* will be number of CPUs
  - Number of connections = 50
  - Test duration = 5 minutes (300 sec)
- By default the results are stored in the Hasura GraphQL Engine used for benchmarking.

### Storing results ###
- The results are stored in schema `hge_bench`.
- For schema, see file `results_schema.yaml`
- The main table is `hge_bench.results`. This table stores the following details
  -  *cpu_key*: This is a foriegn key reference to *cpu_info(key)*. The table *cpu_info* captures the various parameters of the CPU inwhich the benchmark was run, including the model and number of vCPUS
  - *query_name*: This is a forieng key reference to *gql_query(name)*. The table *gql_query* stores the name of the query and the query itself used for tests.
  - *docker_image*: Stores the docker images of Hasura GraphQL Engine when the HGE is run as docker
  - *server_shasum*, *version*: These are stored when HGE is run with `cabal run`. Version stores the version generated by script *gen-version.sh*. The *server_shasum* stores the shasum of the files in the server folder (excluding tests folder). This shasum shows whether the server code has actually varied between the commits.
  - *postgres_version* : Stores the version of Postgres
  - *latency*, *requests_per_sec*: Stores the benchmark latency and requests\_per\_sec results
  - *wrk_parameters*: Stores the parameters used by wrk during benchmarking, including number of threads, total number of open connections, and duration of tests
