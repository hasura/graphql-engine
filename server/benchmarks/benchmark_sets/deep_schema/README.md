This data set is meant to test nested remote relationships: it contains a huge
amount of remote relationships, chosen to make it so that as "depth-first"
traversal will cross from one source to the other 50 times. Building the schema
across remote relationships isn't easy, and the reason this set came to be was
that an approach we considered for how to use different contexts for different
sources actually resulted in a performance degradation with deep schemas such as
this artificially created one.

At time of writing, we have no plan to use this set to benchmark queries: our
main concern is the schema building time, as measured by a call to
`replace_metadata`.

A degradation of performance that is made visible by this set but no other
likely indicates a problem with the implementation of remote relationships in
the schema.
