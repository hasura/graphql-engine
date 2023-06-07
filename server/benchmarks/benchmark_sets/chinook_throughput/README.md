This is a copy of our Chinook latency benchmarks, but for measuring maximum
sustained throughput, rather than latency at different load levels.

Benchmark sets like this one which have "throughput" in the name will be
displayed differently in our regression report. We keep the same query names as
in the regular Chinook benchmarks, so they can be easily compared using e.g.
https://hasura.github.io/graphql-bench/app/web-app/#mono-pr-1234/chinook,mono-pr-1234/chinook_throughput

This is also used by `resource_calibration.sh` to get a peak through but number
for the purposes of understanding resource provision.
 
