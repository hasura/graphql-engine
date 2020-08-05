.. meta::
   :description: Using Hasura's Relay API
   :keywords: hasura, docs, Relay, schema, API

.. _relay_schema:

Relay schema
============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

The Hasura GraphQL engine serves a `Relay <https://relay.dev/>`__ schema for Postgres tables which have a primary key defined. The Relay schema can be accessed through the ``/v1beta1/relay`` endpoint.

.. thumbnail:: /img/graphql/manual/schema/relay.png
   :alt: Relay API toggle

.. admonition:: Supported from

  Relay is supported from versions ``v.1.3.0`` and above.

What is Relay?
--------------

Relay is an opinionated JavaScript framework for declaratively fetching and managing GraphQL data. Relay's strength lies in how it removes opportunities for developer errors, by providing conventions that result in performant and type-safe apps.

While using Relay saves you work on the client side with tasks like pagination, using Relay and Hasura together saves you even more work, because Hasura automatically sets up the Relay backend for you on top of your database.

Benefits of Relay
-----------------

Relay's client-side benefits include:

- **Colocation**: By convention, GraphQL fragments are colocated with their views, so that each component describes exactly what data it needs. This declarative approach has several benefits:

  - It's hard to over-fetch data (which would hurt performance), or under-fetch data (which might cause errors).
  - Components can only access data they've asked for. This **data masking** prevents implicit data dependency bugs.
  - Components only re-render when the exact data they're using is updated, preventing unnecessary re-renders.

- **Performance**: The Relay compiler composes your GraphQL fragments into optimized and efficient batches to reduce round-trips to the server. The compiler also applies `transforms <https://relay.dev/docs/en/compiler-architecture.html#transforms>`__ to your queries to remove redundancies and shorten query strings, which reduces upload bytes.

- **Strong typing**: The compiler automatically generates Flow (or TypeScript) types, which you can import into your component for type checking in your code editor and during build time.

To learn more about these and other Relay features, like persisted queries, local state management, passing arguments to fragments, and fetching data as early as possible, check out the `Relay docs <https://relay.dev/docs/en/experimental/a-guided-tour-of-relay>`__.

.. note::
  For a more detailed breakdown of Relay and its benefits, check out our `deep-dive on Relay <https://hasura.io/blog/deep-dive-into-relay-graphql-client/>`__.

Relay's server spec
-------------------

To support the above client-side benefits, Relay has a particular server specification.

*Hasura's Relay API sets up this spec for you automatically, so you don't have to implement it manually.*

According to the spec, the server must provide:

- **A mechanism for refetching an object**: The convention is a **Node interface** with a globally unique ``id`` field, as well as a root field called ``node``, which allows fetching data by this ``id``. This is great for performance on the client side, but hard to implement on the server side, since you have to make sure  ``id``'s are globally unique, objects can be re-fetched via their ``id``'s, etc.

- **A description of how to page through connections**: Connections are Relay's way of **standardizing pagination**. They allow us to communicate more info between the client and the server, such as cursors and page info, so that we can paginate in a predictable pattern. On the client side, Relay saves you a ton of work by keeping track of the moving parts of pagination and merging results automatically.

.. note::
  Check out this `example repo <https://github.com/hasura/graphql-engine/tree/master/community/sample-apps/react-relay>`__ to see how to set up pagination with Hasura and Relay.

Limitations
-----------

At this time, Hasura's Relay implementation only supports Postgres tables with a primary key defined, and custom SQL functions whose returning table has a primary key defined.

Persisted queries will be supported soon.

.. note::

  Currently, Hasura's Relay schema doesn't expose remote schemas or actions. This will be fixed in future releases.
