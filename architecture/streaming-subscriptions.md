# Instant streaming APIs with built-in authorization for new or existing Postgres

This post discusses a recent capability added to Hasura GraphQL Engine: Streaming over GraphQL subscriptions for Postgres.

## TL;DR 

If you have a large amount of data, or "fast moving" data in Postgres, Hasura now allows you to instantly create an API for clients to fetch that data as a continuous stream. This API can be safely exposed to internal or external HTTP clients.

- Uses GraphQL subscriptions, and works with any GraphQL client that supports subscriptions over websockets
- Each client can maintain an independent stream cursor (aka offset) and prevent any dropped or missing events - aka not fire-and-forget
- Each client can only read relevant events in a stream: Create fine-grained authorization rules at a "row" (or event) and "column" (or field) level, that integrates with any authentication provider
- Use relationships to enrich event payload data with data in other models at query time
- Scale to a massive number of concurrent clients. This post discusses a benchmark of over 1M clients concurrently streaming data with **independent stream offsets** and **independent authz rules** 
- Works with new or existing data in any Postgres database and with read-replicas
- No special configuration around sticky sessions, back-pressure, dropped connections, roll-outs, scaling out etc required

![Streaming Subscription Getting Started](https://graphql-engine-cdn.hasura.io/assets/blog/streaming-subscriptions/streaming-subs.gif)

## Motivation

Today we have a variety of solution to ingest and store a large amount of data or a stream of data. 
However, once this data has been captured, securely exposing this data as a continuous stream to a large number of HTTP clients concurrently is a challenge. 

These are the challenges that Hasura aims to address:
1. Allow for authorization rules so that clients can only consume the subset of the stream they have access to
2. Prevent missing events and allow each client to move through its stream independently
3. Scale and manage websocket connections to support hundreds of thousands to millions of HTTP clients concurrently 

Hasura's new streaming API on Postgres addresses these challenges so that teams focus on how their streams are modelled and secured instead of building and scaling the API.

![Without Hasura](https://graphql-engine-cdn.hasura.io/assets/blog/streaming-subscriptions/motivation-before-hasura.png)
![With Hasura](https://graphql-engine-cdn.hasura.io/assets/blog/streaming-subscriptions/motivation-after-hasura.png)

## Table of contents

In this post, we will look at the architecture of streaming subscriptions, how our batching and multiplexing works with declarative authorization. We will take a look at initial performance benchmarks that test for high-concurrency and volumes of data. And finally, we’ll highlight a few common use-cases and modeling patterns. 

- [Try it out](#try-it-out)
- [Common use-cases](#common-use-cases-and-modelling-guides)
- [Architecture](#architecture)
    - [How does streaming subscriptions work?](#how-does-streaming-subscriptions-work)
    - [How does streaming subscription work for a single subscriber?](#how-does-streaming-subscription-work-for-a-single-subscriber)
    - [Batching and Multiplexing](#batching-multiple-streaming-subscriptions-into-one-sql-query)
- [Handling Authorization with large data](#handling-authorization-with-large-data)
- [Backpressure and Scaling](#handling-backpressure-and-scaling)
- [Performance Benchmarks](#performance-benchmarks)
- [Next steps](#next-steps)

## Try it out in 60 seconds

1. Step 1: Deploy Hasura as a [docker image](https://hasura.io/docs/latest/getting-started/docker-simple/) or on [Hasura Cloud](https://cloud.hasura.io)
2. Step 2: Connect a Postgres database to Hasura as a source:
  - Connect a new or existing Postgres database to it
3. Step 3: Track an existing table, or create a new table with an monotonically increasing, unique id (eg: bigserial).

```sql
CREATE TABLE public.message (
  id integer NOT NULL,
  username text NOT NULL,
  text text NOT NULL,
  "timestamp" timestamp with time zone DEFAULT now() NOT NULL
);
```

4. Step 4: Run a GraphQL subscription to try out the streaming API on that table

```graphql
message_stream {
  id
  username
  text
}
```

## Use-cases

### Stream log-like data

Let's say you have log-like data that is being continuously generated and needs to be streamed to web clients. 

What you need to do:
1. Process and ingest data into a Postgres cluster
2. Run Hasura and add your Postgres (primary & read-replicas) as sources
3. Set up authorization rules in Hasura to decide who can stream what data.
   ```
   Read log if log.service.viewers contains session.user_id
   ```

Architecture examples:
- Ingest data into a Kafka-like queue, which is processed and written to a global "edge" AWS Aurora Postgres cluster

![AWS Aurora Postgres](https://graphql-engine-cdn.hasura.io/assets/blog/streaming-subscriptions/logs-use-case-I.png)

- Ingest data into TimescaleDB, setup a continuous materialized view and stream realtime aggregations

![TimescaleDB realtime aggreations](https://graphql-engine-cdn.hasura.io/assets/blog/streaming-subscriptions/logs-use-case-II.png)

### Create messaging channels

Many use-cases that involve web/mobile clients require an ability to setup a persistent and secure channel that allows clients to publish and subscribe to messages

What you need to do:
1. Create a messages model that has references to channel or groups that determine authorization rules
2. When messages are sent (or published), messages can be ingested directly based on authz rules via Hasura GraphQL mutations, or can be handled by custom logic that processes them and inserts them into the messages channel

- Architecture example:

![Realtime messaging](https://graphql-engine-cdn.hasura.io/assets/blog/streaming-subscriptions/use-case-messages.png)

Note: Messaging channels are a great candidate for sharding by channel_id or other channel metadata. Hasura supports YugaByte, Citus with support for Cockroach coming soon.

### Capture and stream data changes on an existing table

What you need to do:
1. Create an audit trigger that captures data from the table, and inserts the change event into a changes table
2. Setup authorization rules on the changes table so that only changes to the authorized rows and the right subset of columns are streamed

- Architecture:

![Data Capture](https://graphql-engine-cdn.hasura.io/assets/blog/streaming-subscriptions/use-cases-CDC.png)

Notes on preventing load on the primary:
- Use logical replication to replicate a subset of the data to a secondary Postgres
- Set up triggers on the second Postgres as suggested above

Note on capturing data from the WAL:
- LR slots are expensive. Maintaining unique cursors and authorization rules for each HTTP client can get prohibitively expensive. Instead, capture data from the WAL and insert that into a flattened table in a second Postgres. This table will now contain the entire WAL, and Hasura can help clients stream data from that table.

### Create a fire-and-forget channel for ephemeral data

Typing indicators, live locations sharing multiplayer mouse-pointer like information are ideal candidates for fire-and-forget type channels. These types of applications usually require lower e2e latency as well.

Architecture:
- Create (or alter) an UNLOGGED table in Postgres
- Set up a authorization rule (optimize the authorization predicate so that the validation can be done with data within the same row)

![Ephemeral](https://graphql-engine-cdn.hasura.io/assets/blog/streaming-subscriptions/use-case-ephemeral.png)

Note: The advantage of using Postgres is that we can leverage its query engine for authorization (via Hasura's predicate push down) and independent cursors per stream. Furthermore, the data retention policy can also be fine-grained and controlled quite easily. Furthermore, given that Hasura allows adding multiple Postgres instances to the same GraphQL Engine configuration, we can tune an entirely separate Postgres server to trade increased read & write performance for data-loss.

## Notes on architecture

Hasura already supports the ability to subscribe to the latest value of a query (aka a live query) over GraphQL subscriptions, which works well for use-cases that don't need a continuous stream of data or events. Eg: the current set of online users, the latest value of an aggregate etc. Read more about how these live queries work in Hasura [here](https://github.com/hasura/graphql-engine/blob/master/architecture/live-queries.md).

Streaming however, is a different GraphQL subscription field that is now provided by Hasura, ideal for streaming a large result set continuously or for consuming events continuously in a robust way.

### API design goals

These were the goals and constraints we put on the design of the API:
1. Should work with GraphQL subscriptions out of the box
2. Should compose well with Hasura's existing relatonships and authorization system
3. Should allow the client to quote its own offset

### Making GraphQL calls efficient

Before we dive in to the specifics, let us look at how Hasura handles an incoming subscriber, ie. a GraphQL query.

Part of Hasura is a transpiler that uses the metadata of mapping information for the data models to the GraphQL schema to “compile” GraphQL queries to the SQL queries to fetch data from the database.

GraphQL query → GraphQL AST → SQL AST → SQL

![GraphQL Query AST SQL](https://graphql-engine-cdn.hasura.io/assets/github/graphql-query-ast.png)

This gets rid of the N+1 query problem and allows the database to optimise data-fetching now that it can see the entire query.

Let’s apply this transpiler to streaming subscriptions.

![Streaming Subscriptions Architecture](https://graphql-engine-cdn.hasura.io/assets/github/streaming-subscription-architecture.png)

The SQL is run with current cursor value, the response is sent to the subscriber and once the response is processed, the cursor value is updated for the next data set. This works well for a single subscriber for a given query.

### Batching multiple streaming consumers into one SQL query

When there are a multiple connected clients that perform streaming subscriptions, the graphql-engine "multiplexes" subscriptions
that are similar.

For example:

Let's say `n` clients run the following streaming subscription with different values of `id`.

```graphql
subscription StreamLogs ($id: Int!) {
  logs_stream(initial_value: {id: $id}, batch_size: 10) {
    log_type
    log_level
    log_data
  }
}
```

The SQL query generated for the above GraphQL query will look something like

```sql
SELECT log_type, log_level, log_data, MAX(id) as cursor
FROM
( SELECT * FROM logs WHERE id > $id LIMIT 10 )
```

We can see that even if there are `n` different subscriptions being run, they all run the same
generated parameterized SQL query. The graphql-engine now leverages this by running one single parameterized query with `n` parameters.

This is done by "joining" the SQL query to an array of `id` values and the response then returns `n` rows
and each row corresponding to an output to a distinct value of `id`.

By "multiplexing" subscribers, the number of DB connections will be lesser than the number 
of subscriptions.

## Handling authorization with a predicate push-down

We have seen how Hasura makes performant queries. But what about Authorization?

The naive approach would be to fetch the data from the database (involves IO), apply authorization checks for each element in the response (involes IO and compute) and then send the result back to the client. The problem with this approach is that the initial data fetch is not definitive. The data is being filtered based on rules post the fetching from the database and as data becomes bigger, compute and latency becomes high and so does the load on the database.

It is impossible to load large streams of data in memory and apply authorization rules for filtering data before sending it back to the client. The Hasura approach to this problem is two fold. Make the data fetching performant and make authorization declarative by applying them at the query layer.

We just saw above how the transpiler in Hasura with batching helps in making performant queries. But this in itself isn't enough as resolvers also enforce authorization rules by only fetching the data that is allowed. We will therefore need to embed these authorization rules into the generated SQL.

### Make Authorization declarative

Authorization when it comes to accessing data is essentially a constraint that depends on the values of data (or rows) being fetched combined with application-user specific “session variables” that are provided dynamically. For example, in the most trivial case, a row might container a user_id that denotes the data ownership. Or documents that are viewable by a user might be represented in a related table, document_viewers. In other scenarios the session variable itself might contain the data ownership information pertinent to a row, for eg, an account manager has access to any account [1,2,3…] where that information is not present in the current database but present in the session variable (probably provided by some other data system).

Authorization is declarative and available at a table, view or even a function (if the function returns SETOF) level. It is possible to create a single SQL query that has these authorization rules added to it as additional predicateds in the query (additional clauses to the WHERE clause).

The resultant query processing pipeline is now: GraphQL query → GraphQL AST → Internal AST with authorization rules → SQL AST → SQL

![GraphQL query AST with AuthZ](https://graphql-engine-cdn.hasura.io/assets/github/graphql-query-ast-authz.png)

### Backpressure

Backpressure is the mechanism that “pushes back” on the producer to not be overwhelmed by data.

While streaming, it can happen that the server keeps on sending new events to a client while the client already has a huge backlog of events to process. In such a case, when the client is overwhelmed with a large amount of events and is not able to cope up with the events sent by the server, then the client should manually disconnect the subscription noting down the last cursor value. Once the client is ready to accept new events it can start a new streaming subscription from the last cursor value it processed.

### Scaling 

Hasura’s subscriptions can easily be scaled horizontally by adding more Hasura instances and can also be scaled at the DB layer by adding read-replicas to the primary database. 

## Performance Benchmarks

Read more about the benchmark setup here - [https://github.com/hasura/streaming-subscriptions-benchmark](https://github.com/hasura/streaming-subscriptions-benchmark)

We use a similar schema and authorization rule setup as in the "messaging channels" use-case above.

Our aim in this benchmark is to verify that Hasura can handle a large number of concurrent subscribers without undue load on the underlying database.
1. Each subscriber has an independent stream. Each maintains its own offset, and connects with a different authorization rule. 
2. We scale from 20k concurrent to 1M concurrent and observe:
  a. Load on Hasura instances
  b. Database CPU load
  c. Number of database connections required
3. Configuration:
  a. Database: Single Postgres instance (RDS - 16x CPU, 32GB RAM)
  b. Hasura: 100x instances on App runner (4xCPU, 8GB RAM)

Here is how the load scales:

![DB Connections Load](https://graphql-engine-cdn.hasura.io/assets/blog/streaming-subscriptions/performance-db-conn-load.png)

- DB CPU percentage at peak usage was 15%
- Peak Hasura CPU usage was 20%

## Next steps:

If you have a streaming workload, try Hasura out and feel free to reach out to us for questions, feedback and for any other architectural discussions. 

- Try it out: [Hasura Cloud](https://cloud.hasura.io) or with [Docker](https://hasura.io/docs/latest/getting-started/docker-simple/)
- Reach out to us [Discord](https://hasura.io/discord) or [Github Discussions](https://github.com/hasura/graphql-engine/discussions)
- [Contact Us](https://hasura.io/contact-us/)