This is a repro case for the bad memory usage behavior scenario described in
https://github.com/hasura/graphql-engine-mono/pull/9447 .  see that thread for
details.

This is just here as a runnable example of the workload; the numbers we get
back from the benchmark are not really useful, since e.g.  many of the queries
actually timeout.

One thing that wasn't fixed in #9823, and for which this is a good repro case:
The server has no ability to detect when a client has disconnected in the
middle of an ongoing query, for example because the client library timed out,
and likewise does not cancel the corresponding long-running database query. 

