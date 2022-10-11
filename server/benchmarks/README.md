This is our v2 benchmark suite, which gets run in CI. It makes use of
[graphql-bench](https://github.com/hasura/graphql-bench) internally (which in
turn uses the [K6 load testing tool](https://k6.io/))

Here is an overview of this directory:

### benchmark_sets/*

Each sub-directory here contains a different schema and accompanying latency
benchmarks (see below for adding your own). Each benchmark set runs in parallel
on CI. See `benchmark_sets/chinook/` for reference.

In a benchmark set directory, the existence of an empty file named one of the
following have the following effects:

- `SKIP_CI`: don't run the benchmark in CI at all
- `SKIP_PR_REPORT`: don't post the regression results directly in the PR
  comment body (useful if the results are noisy for now)

### benchmark_sets/*/adhoc_operations/*

Each script here (if present) defines a non-graphql operation to be
benchmarked. These are run in sequence after the graphql-bench -powered queries
defined in the `config.query.yaml` files.

These are suitable for e.g. metadata queries or more complex interactions with
the server, where you don't care about concurrent requests and where the work
is CPU-bound (because in the future reporting will highlight stable metrics
like CPU mutator time and allocations).

Because of the overhead of timing things in bash you probably don't want to
rely on the wallclock latency numbers for queries that are faster than 100 ms.
These numbers may also be noisier since we're running single-threaded and so
taking fewer samples.

### bench.sh

This script runs a particular benchmark set against a particular hasura docker
image or, if omitted, the hasura running on port 8181. e.g. to run the
`chinook` benchmarks:

    $ ./bench.sh chinook hasura/graphql-engine:v2.0.1

Only fairly recent (as of this writing) builds of hasura are supported.

Be aware benchmarks are tuned for our beefy CI machines, and some may perform
poorly and give useless output on a laptop with few cores.

### fabfile.py

This is the core of the CI functionality. It's possible to run this locally
but you'll need credentials (see `.circleci/config.yaml`). In general this can
be ignored.

---

## Interpreting benchmark results

- **bytes_alloc_per_req should be very stable** but of course doesn't measure,
  e.g. whether we're generating efficient SQL

- **min latency is often stable**, especially when we have many (>5,000) samples; a
  regression here may mean a change to the code is influencing performance
  ...or it might **indicate the test run was unstable** and should be taken
  with a grain of salt, or retried

- ...but **[long tail latencies](https://engineering.linkedin.com/performance/who-moved-my-99th-percentile-latency)**
  are very important; the 90th percentile may be a better target to optimize for
  than the median (50th percentile)

- ...but keep in mind that **longtail latencies are by definition noisey**: the
  99.9th percentile may represent only a handful of samples. Therefore be
  cautious when drawing inferences from a _comparison_ of tail latencies between
  versions.

- If **Memory Residency** has changed:
  - `live_bytes` is just the total size of heap objects after GC, and is quite
    deterministic; `mem_in_use` is closer to what users experience in their nice graphs
  - does the regression show up in `huge_schema`? If not maybe a function was
    turned into a CAF. Small, constant memory usage increases are probably not
    a big deal

    The following is good background for `mem_in_use` vs. `live_bytes`, and why
    we might care:

    https://well-typed.com/blog/2021/01/fragmentation-deeper-look/
    https://well-typed.com/blog/2021/03/memory-return/

- If optimizing or tuning the output/compression codepath:
  - `chinook`.`*_small_result` and `simple_query_*` queries are average wrt
    response body size (according to cloud data)
  - ...and `chinook`.`full_introspection` is ~P95

## Adding a new benchmark and reviewing

You'll create a new directory under `benchmark_sets/`, and in general can
follow the pattern from `chinook`. The process looks like:

- `export_metadata` to create your `replace_metadata.json` using _stable
  hasura_ if possible, so that you can compare performance against older
  versions of hasura (e.g. `chinook` and `huge_schema` use v2 of metadata)

- check `major_gcs` from `/dev/rts_stats` before and after a test run, ensuring
  the benchmark ran for long enough to perform at least a few major GCs.

- play with benchmark duration, so that results are repeatable but take no
  longer than necessary (see also above)

- if you're interested in latency, be sure you haven't requested a rate too
  close to the throughput limit; experiment locally to find an appropriate
  upper bounds for load.

- set `preAllocatedVUs` value high enough so that K6 doesn't have to allocate
  VUs during test, and you see no `dropped_iterations` reported

- First set `discardResponseBodies: false`, look for `✓ no error in body` in K6
  output to make sure your query is correct (assuming you're not benchmarking
  error handling). Then set `discardResponseBodies: true` so that we're not 
  measuring any processing (like body decompression) performed by K6.

- document the purpose of the benchmark. e.g. "large response bodies at high
  throughput", or "complex joins with conditions on a table with a lot of data,
  ensuring we're generating an efficient query"; give context so your fellow
  developers have a sense of what a regression means

Make sure the benchmark set takes **less than 20 minutes**, otherwise it will
be killed. You can always split benchmarks up into two different sets to be run
in parallel.

If a benchmark set is not fairly stable, or you're not sure if it is, add an
empty file named `SKIP_PR_REPORT` in the benchmark set's directory; this will
prevent display of regression numbers in the PR comment body, but will still
run and record the benchmarks.

**For code reviewers**: help double-check the above, in particular look for
errors in K6 output. Take a look at the detailed GUI report, make sure if using
`constant-arrival-rate` that the query can keep up with load.
