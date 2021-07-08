This is our v2 benchmark suite, which gets run in CI. It makes use of
[graphql-bench](https://github.com/hasura/graphql-bench) internally (which in
turn uses the [K6 load testing tool](https://k6.io/))

Here is an overview of this directory:

### benchmark_sets/*

Each sub-directory here contains a different schema and accompanying latency
benchmarks (see below for adding your own). Each benchmark set runs in parallel
on CI.

### bench.sh

This script runs a particular benchmark set against a particular hasura docker
image or, if omitted, the hasura running on port 8181. e.g. to run the
`chinook` benchmarks:

    $ ./bench.sh chinook hasura/graphql-engine:v2.0.1

Only fairly recent (as of this writing) builds of hasura are supported.

Be aware benchmarks are tuned for our beefy CI machines, and some may perform
poorly and give useless output on a laptop with few cores.

### fabfile.py

This is the core of the CI functionality. It's possibile to run this locally
but you'll need credentials (see `.circleci/config.yaml`). In general this can
be ignored.

---

## Interpreting benchmark results

- **bytes_alloc_per_req should be very stable** but of course doesn't measure,
  e.g. whether we're generating efficient SQL 

- **min latency is often very stable**, especially when we have many (>5,000) samples; a
  regression here very likely means a change to the code is influencing performance
  ...or it might **indicate the test run was unstable** and should be taken
  with a grain of salt, or retried

- ...but **[long tail latencies](https://engineering.linkedin.com/performance/who-moved-my-99th-percentile-latency)**
  are very important; the 90th percentile may be a better target to optimize for
  than the median (50th percentile)

- ...but keep in mind that **longtail latencies are by definition noisey**: the
  99.9th percentile may represent only a handful of samples. Therefore be
  cautious when drawing inferences from a _comparison_ of tail latencies between
  versions.

## Adding a new benchmark

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

- set `preAllocatedVUs` juar high enough so that K6 doesn't have to allocate
  VUs during test, and you see no `dropped_iterations` reported

- look for `✓ no error in body` in K6 output to make sure your query is correct
  (assuming you're not benchmarking error handling)

- document the purpose of the benchmark. e.g. "large response bodies at high
  throughput", or "complex joins with conditions on a table with a lot of data,
  ensuring we're generating an efficient query"; give context so your fellow
  developers have a sense of what a regression means

Make sure the benchmark set takes **less than 20 minutes**, otherwise it will
be killed. You can always split benchmarks up into two different sets to be run
in parallel.
