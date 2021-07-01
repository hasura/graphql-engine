.. meta::
    :description: Hasura FAQs
    :keywords: hasura, docs, FAQs

.. _faq:

FAQs
====

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

.. _faq_db: 

What types of workloads and databases does Hasura support?
----------------------------------------------------------

Hasura supports the following databases: 
    - PostgresQL (and the Postgres family of databases: Yugabyte, Timescale, Citus, Aurora)
    - SQL Server
    - Big Query

.. _faq_how_hasura_works:

How does Hasura work?
---------------------

Although Hasura presents itself as a web-service, 
Hasura is quite a JIT compiler, Hasura takes incoming GraphQL API 
calls over HTTP and then tries to achieve theoretically optimal performance 
while delegating the data fetches to downstream data-sources. You can read 
more about Hasura’s design philosophy in this `blog post. <https://hasura.io/blog/how-hasura-works/>`__

.. _faq_hasura_timesaving:

How much time & effort does Hasura save?
----------------------------------------

Hasura cuts down development time by 50-80%. You can find out more from 
our case studies `here. <https://hasura.io/user-stories/>`__ 

.. _faq_hasura_existing_app_api:

How do I use Hasura if I already have an existing application or API?
---------------------------------------------------------------------

Hasura is designed for incremental adoption without having to 
rip-and-replace or entirely rebuild your stack. You can incrementally 
migrate your application to Hasura. Use Hasura to first build any new 
features for your application using your existing data as well as a high 
performance read layer for any read heavy use-cases as this takes no time 
to set up. You can also use any business logic in your existing applications 
by delegating to them via Hasura Actions. This will give you the time to
migrate over any legacy code or rewrite existing microservices with Hasura. 

.. _faq_business_logic:

Where do I put business logic?
------------------------------

Hasura exposes your domain’s data and logic over a high-performance flexible API. If 
you’re pointing Hasura at a database you might be wondering, where you’d write the 
necessary business logic that’s required for an API.

Note that Hasura removes the need for writing any code required for external or internal 
authorization rules. 

Hasura provides 4 ways for exposing existing or new business logic in your domain:

**Event triggers:** 
Whenever there’s a change in the upstream database, Hasura can 
capture that change as an event and deliver that to a HTTP webhook that can process 
that data change event and react to it asynchronously. Apart from attaching specific pieces 
of logic to events, this is especially useful if you’re thinking about building end to end 
realtime and reactive applications. 

- Read more about this architecture at https://3factor.app
- Read more about event triggers in the Hasura :ref:`docs. <event_triggers>`
- Go through a quick tutorial on how to set event triggers up at https://learn.hasura.io

**REST APIs:** 
If you have new or existing REST APIs that serve domain data or logic, you can easily connect 
Hasura to them and extend the GraphQL schema that Hasura exposes. This is useful not just when 
you have legacy APIs that contain a lot of transactional or application logic, but also when you 
want to build and serve custom logic with cloud-native code deployed as containers or serverless 
functions.

- Read more about :ref:`Hasura Actions <actions>`

**GraphQL APIs:**
If you have a new or existing GraphQL service that extends the schema, say with custom mutations 
that incorporate your custom logic, or if you’d like to extend your overall GraphQL API with a 
“sub graph” that comes from a service which you may not directly own, you can use “Remote Schemas” 
in Hasura to bring in GraphQL services as data & logic providers to your GraphQL API.

- Read more about :ref:`Remote Schemas <remote_schemas>`

**Stored procedures / functions in the database:**
Stored procedures and functions are a common way to write and store high-performance business 
logic, or transactional logic, that is close to the data. As a part of the GraphQL API that Hasura 
exposes over databases, Hasura allows you to expose stored procedures or functions as fields in 
the GraphQL schema. This is a great way to bring in existing business logic that maybe in your 
database, or to write custom, high-performance logic if you’re familiar with databases!

- Read more about :ref:`custom functions <custom_sql_functions>`

Choose one or more of the methods above depending on where your existing business logic is, and 
where you want it to be in the future. 

For example, you might have existing logic in synchronous REST APIs in Java or .NET, but you 
might want to write new logic as reactive event triggers deployed as serverless functions (or 
lambdas) in Javascript or Python or Go!

.. _faq_REST_api:

Can I use REST instead of GraphQL APIs?
---------------------------------------

Hasura 2.0 added support for REST APIs. Hasura 2.0 allows users to create idiomatic REST endpoints based on GraphQL templates. Read more :ref:`here. <restified_api_reference>`

.. _faq_hasura_auth:

Can Hasura integrate with my authentication system?
---------------------------------------------------

Hasura believes authentication should not be restricted to a particular provider, hence, we 
make it really easy for you to bring your own authentication system. The most favoured 
mechanism is via JWT. Hasura can accept JWT tokens from any standard JWT provider. 
For extremely customized authentication systems, Hasura also supports auth webhook that 
allows you to read through cookies or tokens that might have a custom format. We have guides 
for some of the popular authentication providers.
Read more :ref:`here. <authentication>` 

.. _faq_hasura_query_caching:

Does Hasura also automatically cache queries or data to improve performance?
----------------------------------------------------------------------------

Hasura GraphQL Engine supports Query Caching automatically, where the internal 
representation of the fully qualified GraphQL AST is cached. 

When hitting the database, the generated SQL of the GraphQl query is a prepared 
statement with the right session variables. These prepared statements help in 
making queries fast and improving performance. 

Response caching (available on Hasura Cloud & Hasura EE) can be enabled by specifying 
which query to cache using the @cached directive. Read more about caching `here. <https://hasura.io/learn/graphql/hasura-advanced/performance/1-caching/>`__

.. _faq_ABAC_RBAC:

How does Hasura handle ABAC, RBAC style authorization policies?
---------------------------------------------------------------

Hasura implements RBAC by automatically publishing a different 
GraphQL schema that represents the right queries, fields, and 
mutations that are available to that role. 

For ABAC, session variables can be used as attributes and 
permission rules can be created that can use any dynamic 
variable that is a property of the request.

.. _faq_security:

Does Hasura have other security features, specifically for GraphQL in production?
---------------------------------------------------------------------------------

Hasura has multiple security features to best utilize the power of our GraphQL Engine.
Features like service level security, authentication & authorization, allow lists,
rate and response limiting are present. Learn more about Hasura security `here. <https://hasura.io/learn/graphql/hasura-advanced/security/>`__


.. _faq_compiler_performance:

How does the compiler approach provide superior performance?
------------------------------------------------------------

Typically when you think of GraphQL servers processing a query, you 
think of the number of resolvers involved in fetching the data for 
the query. This approach can lead to multiple hits to the database with 
obvious constraints associated with it. Batching with data loader can 
improve the situation by reducing the number of calls.

Internally Hasura parses a GraphQL query, gets an internal 
representation of the GraphQL AST. This is then converted to a 
SQL AST and with necessary transformations and variables the final 
SQL is formed.

``GraphQL Parser -> GraphQL AST -> SQL AST -> SQL``

This compiler based approach allows Hasura to form a single SQL query for a GraphQL query of any depth.

.. _faq_scaling:

How does Hasura scale vertically and horizontally?
--------------------------------------------------

Hasura Cloud lets you scale your applications automatically without 
having to think about the number of instances, cores, memory, thresholds 
etc. You can keep increasing your number of concurrent users and the 
number of API calls and Hasura Cloud will figure out the optimizations 
auto-magically. Hasura Cloud can load balance queries and subscriptions
across read replicas while sending all mutations and metadata API calls 
to the master. Learn more about Horizontal scaling with Hasura, `here. <https://hasura.io/learn/graphql/hasura-advanced/performance/2-horizontal-scaling/>`__

.. _faq_slow_api_perf:

How can I improve the performance of slow running API calls?
------------------------------------------------------------

Hasura allows analyzing queries to identify the slow running calls and use 
Indexes to improve the performance. Learn more `here. <https://hasura.io/learn/graphql/hasura-advanced/performance/3-analyze-query-plans/>`__





