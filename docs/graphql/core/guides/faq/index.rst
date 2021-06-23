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
more about Hasura’s design philosophy in this `blogpost. <https://hasura.io/blog/how-hasura-works/>`__

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
by delating to them via Hasura Actions. This will give you the time to 
migrate over any legacy code or rewrite existing microservices with Hasura. 

.. _faq_business_logic:

Where do I put business logic?
------------------------------

Hasura implements an eventing system that provides developers an easy way 
to write stateless business logic that is integrated with Hasura over HTTP.
Hasura captures and delivers events over HTTP and makes certain guarantees 
(at least-once, exactly-once) to help create workflows that allow developers 
to be as productive with stateless business logic as they would have been 
writing code in a stateful monolith!

**Hasura's eventing integrations**:
    - Change Data Capture integrations with data-sources to atomically capture and reliably deliver data events over HTTP. `Read more. <https://hasura.io/docs/latest/graphql/core/event-triggers/index.html>`__
    - API events: API calls made by end-users are mapped to events that are delivered to backing business logic, over HTTP. Hasura can provide a synchronous or asynchronous boundary. `Read more. <https://hasura.io/docs/latest/graphql/core/actions/index.html>`__
    - Time based events: Hasura can trigger events based on a time schedule or a timestamp and deliver them over HTTP to business logic. `Read more. <https://hasura.io/docs/latest/graphql/core/scheduled-triggers/index.html>`__

.. _faq_REST_api:

Can I use REST instead of GraphQL APIs?
---------------------------------------

Hasura 2.0 added support for REST APIs. Hasura 2.0 allows users to create idiomatic REST endpoints based on GraphQL templates. Read more `here. <https://hasura.io/docs/latest/graphql/core/api-reference/restified.html#restified-api-reference>`__

.. _faq_hasura_auth:

Can Hasura integrate with my authentication system?
---------------------------------------------------

Yes, Hasura can integrate with your authentication system, 
including SSO support on our commercial products. We have guides for 
some of the populare authentication providers. Read more `here. <https://hasura.io/docs/latest/graphql/core/auth/authentication/index.html#id1>`__ 

.. _faq_hasura_query_caching:

Does Hasura also automatically cache queries or data to improve performance?
----------------------------------------------------------------------------

Hasura GraphQL Engine supports Query Caching automatically, where the internal 
representation of the fully qualified GraphQL AST is cached. 
When a GraphQL query is made, the generated SQL is a prepared 
statement with the right session variables hitting the database. 
These prepared statements help in making queries fast. 

Response caching can be enabled by users manually by specifying which data to cache
using the @cached directive. Read more about caching `here. <https://hasura.io/learn/graphql/hasura-advanced/performance/1-caching/>`__

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
automagically. Hasura Cloud can load balance queries and subscriptions 
across read replicas while sending all mutations and metadata API calls 
to the master. Learn more about Horizontal scaling with Hasura, `here. <https://hasura.io/learn/graphql/hasura-advanced/performance/2-horizontal-scaling/>`__

.. _faq_slow_api_perf:

How can I improve the performance of slow running API calls?
------------------------------------------------------------

Hasura allows analyzing queries to identify the slow running calls and use 
Indexes to improve the performance. Learn more `here. <https://hasura.io/learn/graphql/hasura-advanced/performance/3-analyze-query-plans/>`__





