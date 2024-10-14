# SQL Interface

An experimental SQL interface over OpenDD models. This is mostly targeted at AI
use cases for now - GenAI models are better at generating SQL queries than
GraphQL queries.

This is implemented using the Apache DataFusion Query Engine by deriving the SQL
metadata for datafusion from Open DDS metadata. As the implementation currently
stands, once we get a `LogicalPlan` from datafusion we replace `TableScan`s with
NDC queries to the underlying connector. There is a rudimentary optimizer that
pushes down projections to the opendd query so that we don't fetch all the
columns of a collection.
