# Hasura + SQL (OLAP)

What do people like doing with SQL OLAP DBs?

- Use SQL to extract business insights from their data
  - SQL syntax and power is very important
- Since data is not frequently written and comes from another source consistency is assumed once data is in the OLAP store
  - Physical data modeling, normalization is not important
  - Joins are not important, data is often denormalized

The Hasura + X story:

- Hasura makes OLAP stores high-concurrency and low-latency
  - Query batching
  - Caching
- Hasura brings end-user authz to aggregations API (on single tables)
- Hasura brings out the best of OLAP features using PMs

**What Hasura needs to have to make Hasura + OLAP CDWs awesome:**

1. All single model features
2. Aggregations roadmap:
   - Group By
   - CTEs, Pivot tables
3. Parameterized models
4. High-concurrency by batching queries per second
5. Low-latency by allowing pre-warm caches
6. Solutions guide to ETL data to a row-storage engine instead of columnar
7. Async queries for slow running queries

**Clickhouse specifics over and above:**

1. Support dictionaries instead of joins
2. Support PKs and skip indices

**Snowflake specifics over and above:**

1. Support time based snapshots in parameterized models
