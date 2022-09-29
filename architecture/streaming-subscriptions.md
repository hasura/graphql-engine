# Instant streaming GraphQL APIs with built-in authorization for Postgres

[Hasura](https://hasura.io) provides a streaming GraphQL API (subscriptions) on your Postgres database, so that you can securely stream data to apps and services with minimal setup. [Try it out](#try-it-out-in-60-seconds)!

![Streaming Subscription Getting Started](https://graphql-engine-cdn.hasura.io/assets/blog/streaming-subscriptions/streaming-subs.gif)

1. **No additional moving parts**:
    - You don't need a custom SDK. There is no duplication of data and no need to deal with consuming events and data from multiple sources of truth (eg: a queue and a persistent DB)
    - You don't worry about missing events, duplicate events or data-synchronization on the client
2. **Fine-grained RLS style authorization**: Control what data and fields in the data are consumable by the client
3. **Scale with Postgres**: Scale reads the way you would scale Postgres reads, and scale writes the way you would scale Postgres ingestion (or in-case of a firehose, ingest into a buffer first).
4. **Zero-ops websockets**: 
    - Hasura handles horizontally scaling your websockets layer, cursor management and authorization with predicate push-down. 
    - Benchmarked to handle 1M concurrent GraphQL clients streaming from 10000 channels with 1M events ingested every minute - on a single instance of Postgres RDS.

## Try it out in 60 seconds

1. Step 1: Deploy with nothing but your browser at [Hasura Cloud](https://cloud.hasura.io) (or run with [Docker](https://hasura.io/docs/latest/getting-started/docker-simple/))
2. Step 2: Run [this SQL (gist)](https://gist.github.com/coco98/803b70ec3fdd48cbe388bd25fcbfcd0e) that creates a table and loads some sample data
3. Step 3: Run a GraphQL subscription to start streaming!
```graphql
subscription {
  messages (cursor: {initial_value: {id: "0"}}, batch_size: 5) {
    id
    message
  }
}
```

## How Hasura makes consuming a stream of data easy

### 1. Consuming a stream at the edge
Exposing your stream data to clients at the edge is hard for the following reasons.

| Challenge | The problem | Hasura approach |
|-----------|-------------|-----------------|
| Authorization | Clients should only be able to consume specific events and specific fields in events | Hasura provides an RLS style authorization policy that allows fine-grained control |
| Sticky sessions & horizontal scaling | When clients or servers drop connections, resuming the stream is hard | Hasura runs as a stateless container and any Hasura instance can resume a dropped connection from any client |
| Historical data & synchronization | A client that is consuming events from a stream, might need access to older data. Cursors need to be synchronized across multiple sources of truth | Hasura provides a streaming API with a cursor-based pagination like abstraction. The only source of truth is Postgres |
| Enriching event payloads with data that is not a part of the event | When a client consumes an event, it might require access to data that is not a part of that event payload and comes through a relationship. Eg: `message.user.email` | Hasura streams over a GraphQL API that allows access to any models that are set up as relationships |
| Missing & duplicate events | Exactly-once delivery is challenging to clients on the edge | Hasura's streaming GraphQL API allows the client to stream starting from a cursor value, almost like paginating and guarantees exactly-once delivery |


### 2. Concurrent writers to an append-only table (optional)
The key constraint that Hasura requires to make consuming a stream easy is by requiring that Hasura is reading from a table that has append-only guarantees while reading.

The order in which data is added to the stream can have different requirements. Concurrent writers into a single table in Postgres (or a queue) can cause events to be added to the stream, in a different order from which they are consumed. 

Publishing events to an append-only table can be done in 2 possible ways.

**Option 1: Allow concurrent writers, but force ordering**
- If you're ingesting data concurrently into Postgres, Hasura provides a trigger that can be attached to your table. This trigger creates an `event_id` which is guaranteed to be a monotonically increasing `bigint` on the table, and is in the order in which they are written. The trigger allows for partitioning the id sequence on any column, in case streams are only consumed from data in the same partition. This is useful, because the ordering guarantee is not provided if you were using a `serial` or using a `timestamp` because concurrent transactions might *commit* in an order that is different from a sequence value or the timestamp value.

**Option 2: Use a single writer with batch loading**
- If you're ingesting data into a queue first then once the data is pulled of the queue and written into Postgres, you can ensure that a single writer batch loads data into the table so that commit order and id orders are maintained. If you have a *firehose* of events being sent to your backend, we recommend ingesting into a queue like a redis-stream or kafka first and then bulk/batch inserting that into an append-only Postgres table.

## Scalability

Scaling streaming is hard. Here are our benchmarks to help you understand the limits of this approach and what to do when you blow past those limits. 

### Consumption (streaming clients)

Hasura is able to effectively map hundreds of GraphQL clients to a single Postgres connection when consuming a stream. Scaling reads is thus a matter of scaling the number of available read connections on Postgres. This is easy to do achieve first by scaling vertically, and then scaling horizontally by adding more read-replicas. The primary cost of scaling with read-replicas is latency, since there is a replication lag from the primary to the replica.

On a single large instance of Postgres RDS, we show that 1M GraphQL clients are easily able to stream data concurrently via this benchmark.

<img src="https://graphql-engine-cdn.hasura.io/assets/blog/streaming-subscriptions/performance-db-conn-load.png" alt="Postgres connection load" width="500px" />

### Ingestion (publishing)

The ingestion bottleneck is Postgres. Broadly there are 3 ways of scaling Postgres writes:
0. Use common Postgres practices to start improving latency (eg: remove constraints, unnecessary indexes etc)
1. Vertically scale your Postgres instance
2. Use a sharded Postgres flavour (eg: Hasura already supports Citus/Hyperscale, Yugabyte. Cockroach support coming soon)
3. Ingest data into a queue like Kafka or a redis stream first, and then bulk/batch insert the data into Postgres

Furthermore, as Hasura adds support for databases that support horizontally scaling out writes (eg: Cassandra), this becomes easier.
Currently however, since all your data is in your own Postgres database, evolving your architecture steadily is tractable!

-------------------------------

## Advanced reading

In this post, we will look at the architecture of streaming subscriptions, how our batching and multiplexing implementation works with declarative authorization. We will take a look at initial performance benchmarks that test for high concurrency and large volumes of data. And finally, we’ll highlight a few common use-cases and modeling patterns. 

- [Try it out](#try-it-out-in-60-seconds)
- [Common use-cases](#use-cases)
  - [Stream log-like data](#stream-events-or-log-like-data)
  - [Create messaging channels](#create-messaging-channels)
  - [Capture and stream data changes on an existing table](#capture-and-stream-data-changes-on-an-existing-table)
  - [Create a fire-and-forget channel for ephemeral data ](#create-a-fire-and-forget-channel-for-ephemeral-data-with-postgres)
- [Architecture](#notes-on-architecture)
    - [Making GraphQL calls efficieint](#making-graphql-calls-efficient)
    - [Batching](#batching-multiple-streaming-consumers-into-one-sql-query)
    - [Handling Authorization with predicate pushdown](#handling-authorization-with-predicate-push-down)
    - [Backpressure and Scaling](#managing-backpressure)
- [Performance Benchmarks](#performance-benchmarks)
- [Next steps](#next-steps)

## Use-cases

### Stream events or log-like data

Let's say you have events or log-like data that is being continuously generated and needs to be streamed to web clients. 

What you need to do:
1. Process and ingest data into a Postgres cluster
2. Run Hasura and add your Postgres (primary & read-replicas) as sources
3. Set up authorization rules in Hasura to decide who can stream what data.
   ```
   Read log if log.service.viewers contains session.user_id
   ```

Architecture examples:
- Ingest data into a Kafka-like queue, which is processed and written to a global "edge" AWS Aurora Postgres cluster

<img src="https://graphql-engine-cdn.hasura.io/assets/blog/streaming-subscriptions/logs-use-case-I.png" width="80%" alt="Distribute logs via a global edge" />

- Ingest data into TimescaleDB, setup a continuous materialized view and stream realtime aggregations

<img alt="TimescaleDB realtime aggreations" src="https://graphql-engine-cdn.hasura.io/assets/blog/streaming-subscriptions/logs-use-case-II.png" width="80%" />

### Create messaging channels

Many web/mobile applications require an ability to setup a persistent and secure channel that allows clients to publish and subscribe to messages

What you need to do:
1. Create a messages model that has references to channel or groups that describe authorization rules
2. Messages can be sent (aka published) directly via Hasura GraphQL mutations (with authz rules) , or can be handled by custom logic that processes them and inserts them into the messages channel

- Architecture example:

![Realtime messaging](https://graphql-engine-cdn.hasura.io/assets/blog/streaming-subscriptions/use-case-messages.png)

Note: Messaging channels are a great candidate for sharding by channel_id or other channel metadata. Hasura supports YugaByte, Citus with support for Cockroach coming soon.


### Capture and stream data changes on an existing table

What you need to do:
1. Create an audit trigger that captures data from the table, and inserts the change event into a "changes" table
2. Setup authorization rules on the changes table so that only changes to the authorized rows and the right subset of columns are streamed

- Architecture:

![Data Capture](https://graphql-engine-cdn.hasura.io/assets/blog/streaming-subscriptions/use-cases-CDC.png)

Notes on preventing load on the primary:
- Use logical replication to replicate a subset of the data to a secondary Postgres
- Set up triggers on the second Postgres as suggested above

Note on capturing data from the WAL:
- LR slots are expensive. Maintaining unique cursors and authorization rules for each HTTP client can get prohibitively expensive. Instead, capture data from the WAL and insert that into a flattened table in a second Postgres. This table will now contain the entire WAL, and Hasura can help clients stream data from that table.

### Create a fire-and-forget channel for ephemeral data with Postgres

Typing indicators, live locations sharing, multiplayer mouse-pointer like information are ideal candidates for fire-and-forget type channels. These types of applications usually require lower e2e latency as well.

Architecture:
- Create (or alter) an UNLOGGED table in Postgres
- Set up a authorization rule (optimize the authorization predicate so that the validation can be done with data within the same row)

![Ephemeral](https://graphql-engine-cdn.hasura.io/assets/blog/streaming-subscriptions/use-case-ephemeral.png)

Note: The advantage of using Postgres is that we can leverage its query engine for authorization (via Hasura's predicate push down) and independent cursors per stream. Data retention can also be fine-grained and controlled quite easily. Furthermore, given that Hasura allows adding multiple Postgres instances to the same GraphQL Engine configuration, we can tune an entirely separate Postgres server to trade increased read & write performance for data-loss.

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

### Handling authorization with predicate push-down

We have seen how Hasura makes performant queries. But what about Authorization?

The naive approach would be to fetch the data from the database (involves IO), apply authorization checks for each element in the response (involes IO and compute) and then send the result back to the client. The problem with this approach is that the initial data fetch is not definitive. The data is being filtered based on rules post the fetching from the database and as data becomes bigger, compute and latency becomes high and so does the load on the database.

It is impossible to load large streams of data in memory and apply authorization rules for filtering data before sending it back to the client. The Hasura approach to this problem is two fold. Make the data fetching performant and make authorization declarative by applying them at the query layer.

We just saw above how the transpiler in Hasura with batching helps in making performant queries. But this in itself isn't enough as resolvers also enforce authorization rules by only fetching the data that is allowed. We will therefore need to embed these authorization rules into the generated SQL.

### Making authorization declarative

Authorization when it comes to accessing data is essentially a constraint that depends on the values of data (or rows) being fetched combined with application-user specific “session variables” that are provided dynamically. For example, in the most trivial case, a row might container a user_id that denotes the data ownership. Or documents that are viewable by a user might be represented in a related table, document_viewers. In other scenarios the session variable itself might contain the data ownership information pertinent to a row, for eg, an account manager has access to any account [1,2,3…] where that information is not present in the current database but present in the session variable (probably provided by some other data system).

Authorization is declarative and available at a table, view or even a function (if the function returns SETOF) level. It is possible to create a single SQL query that has these authorization rules added to it as additional predicateds in the query (additional clauses to the WHERE clause).

The resultant query processing pipeline is now: GraphQL query → GraphQL AST → Internal AST with authorization rules → SQL AST → SQL

![GraphQL query AST with AuthZ](https://graphql-engine-cdn.hasura.io/assets/github/graphql-query-ast-authz.png)

### Managing backpressure

Backpressure is the mechanism that “pushes back” on the producer to not be overwhelmed by data. While streaming, it is plausible that the server keeps on sending new events to a client while the client already has a huge backlog of events to process. Because Hasura is able to handle disconnects elegantly, clients can disconnect from the stream at any point of time (say when an internal counter indicating a backlog of unprocessed events), and reconnect when they're ready. Once the client is ready to accept new events it can start a new streaming subscription from the last cursor value it processed.

### Scaling 

Hasura’s subscriptions can easily be scaled horizontally by adding more Hasura instances. The underlying Postgres database can be scaled vertically and with read-replicas or with sharding (with some [supported Postgres flavours](https://hasura.io/docs/latest/databases/postgres/index/#postgres-flavours)). 

Hasura also supports working with multiple Postgres databases as sources (each with their own schema), it becomes possible to separate out specific workloads into tuned Postgres instances.

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

<img src="https://graphql-engine-cdn.hasura.io/assets/blog/streaming-subscriptions/performance-db-conn-load.png" alt="Postgres connection load" width="500px" />


- DB CPU percentage at peak usage was 15%
- Peak Hasura CPU usage was 20%

## Give us your feedback!

If you have a streaming workload, try Hasura out and feel free to reach out to us for questions, feedback and for any other architectural discussions. 

- Try it out: [Hasura Cloud](https://cloud.hasura.io) or with [Docker](https://hasura.io/docs/latest/getting-started/docker-simple/)
- Reach out to us [Discord](https://hasura.io/discord) or [Github Discussions](https://github.com/hasura/graphql-engine/discussions)
- [Contact Us](https://hasura.io/contact-us/)
