# Overview

### Table of contents

<!--
Please make sure you update the table of contents when modifying this file. If
you're using emacs, you can generate a default version of it with `M-x
markdown-toc-refresh-toc` (provided by the package markdown-toc), and then edit
it for readability.
-->

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

- [High-level architecture overview](#high-level-architecture-overview)
    - [Schema cache](#schema-cache)
    - [Schema parsers](#schema-parsers)
    - [Query execution](#query-execution)
- [High-level code structure](#high-level-code-structure)
    - [Non-Hasura code](#non-hasura-code)
    - [Hasura.Base](#hasurabase)
    - [Hasura.Server](#hasuraserver)
    - [Hasura.Backends](#hasurabackends)
    - [Hasura.RQL](#hasurarql)
        - [Hasura.RQL.DML](#hasurarqldml)
        - [Hasura.RQL.IR](#hasurarqlir)
        - [Hasura.RQL.DDL](#hasurarqlddl)
        - [Hasura.RQL.Types](#hasurarqltypes)
    - [Hasura.GraphQL](#hasuragraphql)
        - [Hasura.GraphQL.Parser](#hasuragraphqlparser)
        - [Hasura.GraphQL.Schema](#hasuragraphqlschema)
        - [Hasura.GraphQL.Execute](#hasuragraphqlexecute)
        - [Hasura.GraphQL.Transport](#hasuragraphqltransport)

<!-- markdown-toc end -->

## High-level architecture overview

This diagram shows the high-level components of the system.

![high-level architecture overview](imgs/architecture.png)

### Schema cache

The _schema cache_ is a live, annotated copy of our metadata that lives in the
Haskell process. It is defined in `Hasura.RQL.Types.SchemaCache`. It contains a
`SourceInfo` for each source that has been added (see
`Hasura.RQL.Types.Source`), which contains all that source's info, such as
information on all tracked tables, tracked functions, connection
information... It also contains information about tracked remote schemas.

Most importantly, it contains the cached version of the schema parsers. More
information in the [Metadata](#metadata) section.

### Schema parsers

We have a unified piece of code that is used to generate the GraphQL schema and
its corresponding parsers at the same time from information in the schema cache
(see our schema cache documentation (TODO: add link) for more information). That
code lives in `Hasura/GraphQL/Schema`. The result is stored back in the schema
cache, as a hashmap from role name to the corresponding "context" (its set of
parsers).

The parsers themselves transform the incoming GraphQL query into an intermediary
representation, defined in `Hasura.RQL.IR`.

### Query execution

When a graphql query is received, it hits the transport layer (HTTP or
Websocket, see `Hasura.GraphQL.Transport`). Based on the user's role, the
correct parser is taken from the schema cache, and applied to the query, to
translate into the IR. We treat each "root field" independently (see `RootField`
in `Hasura.RQL.IR.Root`).

From the IR, we can generate an execution plan: the set of monadic actions that
correspond to each root field; that code lives in
`Hasura.GraphQL.Execute`. After that, the transport layer, which received and
translated the query, runs the generated actions and joins the result.

## High-level code structure

The code structure isn't perfect and has suffered from the heavy changes made to
the codebase over time. Here's a rough outline, that points to both the current
structure and, to some extent, what the structure should be.

### Non-Hasura code

Code outside of the `Hasura` folder (like in `Control` and `Data`) are
standalone libraries that could be open-sourced as they are not specific to our
project, such as `Control.Arrow.Trans`. Most commonly, we extend exiting third
party libraries to add useful missing functions; see for instance:
- `Data.Text.Extended`
- `Control.Concurrent.Extended`

As a rule, those "Extended" modules should follow the following guidelines:
- they re-export the original module they extend, so that the rest of the code can always import the extended library
- they do not depend on any `Hasura` with the (arguable) exception of the `Prelude`

### Hasura.Base

The goal of `Hasura.Base` is to be the place where all "base" code, that do not
depend on any other Hasura code but the Prelude, but that is specific enough
that it doesn't belong outside of the `Hasura`. It currently contains, for
instance:
- `Hasura.Base.Error`, which defines our error-handling code, and the one error
  sum type we use throughout the entire codebase
- `Hasura.Base.Instances`, which defines all missing instances from third-party
  code

More code will be moved in it as we make progress. For instance, tracing and
logging could belong in `Base`, as they are "base" pieces of code that we want
the entire codebase to have access to.

### Hasura.Server

`Hasura.Server` contains all of the network stack, the initialisation code, and
the endpoints' routes. APIs are defined in `Hasura.Server.API`, authentication
code in `Hasura.Server.Auth`.

### Hasura.Backends

Most of the code in `Hasura.GraphQL` is generic, and expressed in a
backend-agnostic way. The code for each backend lives in that backend's
`Hasura.Backends.[Name]` folder. More details in our backend architecture
documentation (TODO: insert link here).

### Hasura.RQL

Before the graphql-engine was the graphql-engine, it supported a JSON-based API
called RQL. A lot of the code still lives in the RQL "primordial soup".

#### Hasura.RQL.DML

This DML is the aforementioned JSON API: this folder contains the data
structures that represent old-style Postgres-only table level Select/Insert/Update/Delete requests.
This API is deprecated, not used by any of our users to our knowledge,
and is only kept in the code because the console still uses it.

#### Hasura.RQL.IR

This is our intermediary representation, into which an incoming query (RQL or
GraphQL) gets translated.

#### Hasura.RQL.DDL

This is where our _Metadata API_'s code lives: the API, as described by
`Hasura.Server.API.Metadata.RQLMetadataV1`, is routed to corresponding functions
in `Hasura.RQL.DDL`.

#### Hasura.RQL.Types

This is a somewhat outdated folder that we need to break down in individual
components. As of now, this folder contains all the "base types" of our codebase
and, despite the folder's name, a lot of the code of the engine's core
behaviour. This includes, among others, most of our metadata code: our
representation types (how do we represent information about tables or columns
for instance) and some of the metadata building process (including all code
related to the internal dependency graph of metadata objects).

### Hasura.GraphQL

This folder contains most of the GraphQL API stack; specifically:

#### Hasura.GraphQL.Parser

This folder is where the schema building combinators are defined. It is intended
as an almost standalone library, see [the Schema Building section](#building-the-schema), used by our
schema code.

#### Hasura.GraphQL.Schema

In turn, this is where the aforementioned parsers are used to build the GraphQL
schema from our metadata. The folder itself contains all the individual
components of our schema: how to construct a parser for a table's selection set,
for a remote schema's input arguments, and so on. It's in the root file,
`Hasura/GraphQL/Schema.hs`, that they're all put together to build the schema;
see more information about this in our [schema documentation](deep-dives/schema.md).

#### Hasura.GraphQL.Execute

While most of the actual translation from the `IR` into a SQL query string is
backend-specific, the top-level processing of GraphQL requests (be they Queries, Mutations or subscriptions) is backend agnostic and lives in `GraphQL.Execute`.
At the time of this writing it also still contains Postgres-specific code that has yet to
be generalized.

Furthermore, it contains all code related to the execution of queries targeting
remote schemas, and high-level code for remote joins.

#### Hasura.GraphQL.Transport

The code in this folder is both the actual entry point of query processing
(`Transport` calls `Execute` which calls the parsers), and the actual execution of
the root fields: in each file (HTTP and WebSocket) is where there's the long
case switch of how to execute each step over the network.
