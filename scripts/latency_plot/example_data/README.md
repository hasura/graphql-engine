This is real data comparing how a larger nursery size affects performance, at
various load levels. Measurements taken from internal timing of `runGQ` logged.
NOTE: service time, not response time which is perhaps the more relevant metric
to look at here! (see wrk2)

Only a single query was used, which returned a fairly large payload from PG.

We see little real benefit (and possible minor regressions) to a large nursery
in this test, except at 1000 RPS load, where the default `-A` leads to
timeouts, and can't sustain that throughput (the result on the graph looks like
a regression, but is actually a sign that more requests completed; this is
where the response time view from wrk2 is really useful, I think).
