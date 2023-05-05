# Multiple backends architecture (historical)

This document was originally written when the project was in its inception. As
of now, it still provides an historical perspective into how the code came to be
the way it is.

This [Google
document](https://docs.google.com/document/d/1YHYSdwcTyXT98H4D0pFuo5Oo__aPTPge8cuvAZTuyhA/edit?usp=sharing)
also documents some of the decision process.

## Table of contents

<!--
Please make sure you update the table of contents when modifying this file. If
you're using emacs, you can generate a default version of it with `M-x
markdown-toc-refresh-toc` (provided by the package markdown-toc), and then edit
it for readability.
-->

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
- [Project overview](#project-overview)
    - [Design goals](#design-goals)
- [Overview](#overview)
    - [The `Backend` typeclass](#the-backend-typeclass)
    - [BackendMetadata](#backendmetadata)
    - [BackendSchema](#backendschema)
    - [BackendExecute](#backendexecute)
    - [BackendTransport](#backendtransport)
- [Implementation challenges](#implementation-challenges)
    - [Heterogeneous containers and existential qualification](#heterogeneous-containers-and-existential-qualification)
    - [Breaking the dependency cycle](#breaking-the-dependency-cycle)
- [Drawbacks, alternatives, open questions, and future work](#drawbacks-alternatives-open-questions-and-future-work)
    - [Too big Backend class](#too-big-backend-class)
    - [Cross-source interactions](#cross-source-interactions)
    - [Growing IR](#growing-ir)
    - [Test suite](#test-suite)

<!-- markdown-toc end -->

## Project overview

The goal of this project is for the GraphQL engine to be able to support multiple backends – that is, to expose in our GraphQL schema tables that are not only stored in Postgres, but also MSSQL, MongoDB, and has many others as we need. This poses several challenges, such as:
- the codebase was originally written with the assumption that the underlying database is Postgres;
- different backends support a different set of features, often incompatible or different in subtle ways.

This document details the chosen approach, its strengths and potential weaknesses.

### Design goals

The two main goals with this design were:
- as much as possible, adding support for a new backend should *not* require changing existing code: it should be enough for a backend to implement the required typeclasses in isolation:
  - this allows for several backends to be worked on in parallel without making it painful to rebase / merge code
  - it encourages having a clean separation in the code between backend-agnostic and backend-specific code
- the code should not explicitly branch on some runtime backend value; it should instead rely on a type-level information:
  - this avoids a proliferation of runtime tests `if backend == Postgres then ...`, favouring typeclasses instead
  - this helps us avoid an entire category of bugs by enforcing at compile time that we don't mix incompatible backend features

A noteworthy drawback: no particular attention was given to the difficulty of onboarding new contributors to the project. The chosen design is quite complex, and over-engineering is a potential failure mode of this approach. This document is one way of trying to make onboarding new contributors easier, by providing an overview, but further attention should definitely be paid to documentation.

## Overview

From a high-level perspective, there are four required steps for a GraphQL query to hit a given backend:
  - metadata must be loaded into the `SchemaCache`: a `SourceInfo` must be created, that contains, among others: a `TableCache`, a `FunctionCache`, and all required connection information;
  - a `GQLContext` must be created from that source: it details what is exposed in the schema, and how do we parse an incoming query (the same code does both since [the Great "Parse, Don't Validate (PDV)" Refactor of 2020](https://github.com/hasura/graphql-engine/pull/4111));
  - the output of the parsers must be representable using RQL's Intermediate Representation (IR): an AST that describes the shape of a given query;
  - the output of the parsers (a `QueryRootField`, `MutationRootField`, or `SubscriptionRootField`) must be translated into a backend-specific query string and be executed over the network.

Our design was inspired was the [Trees That Grow](https://arxiv.org/abs/1610.04799) paper. Namely: most types are decorated with a type parameter that ties them to a specific backend, and we use type families to associate backend-specific types to that parameter whenever required. In total, five typeclasses are required:
- the `Backend` type family,
- a `BackendMetadata` class for creating a `SourceInfo`,
- a `BackendSchema` class responsible for the schema / query parsing,
- a `BackendExecute` class responsible for query planning and translation to the appropriate SQL dialect,
- a `BackendTransport` class for execution over the network.

For a new backend to be supported, it "simply" needs to implement those typeclasses.

### The `Backend` typeclass

Different backends have different column types: our old `PGColumnType` enum is not an appropriate representation for the column types of other backends. The `Backend` typeclass is used to provide a mapping from backend to implementation types. (Additionally, it is where we list all the class constraints on those types, to make other instances easier to write down the line.)

```haskell
class ( Show (TableName  b), Eq (TableName  b)
      , Show (ColumnType b), Eq (ColumnType b)
      ) => Backend (b :: BackendType) where
  type TableName  b
  type ColumnType b

instance Backend 'Postgres where
  type TableName  'Postgres = PGTableName
  type ColumnType 'Postgres = PGColumnType
```

This typeclass is used to generalize metadata types, the IR, and all types that need to contain backend-specific information:

```haskell
-- Metadata
data TableInfo (b :: BackendType) =
  TableInfo { tableName   :: TableName b
            , tableConfig :: TableConfig
            }

-- IR
data AnnDelG (b :: BackendType) v
  = AnnDel { dqp1Table   :: !(TableName b)
           , dqp1Where   :: !(AnnBoolExp b v, AnnBoolExp b v)
           }
```

Another use of the `Backend` typeclass is _extension types_. The IR, by definition, needs to support the union of all possible features we want to support, since any query we accept must be representable by it. Extension types allow us to prune the IR AST for a given backend, by making some features unrepresentable, or to add backend-specific information to the AST if required.

```haskell
instance Backend 'MSSQL where
  type XRelay 'MSSQL = Void

data AnnFieldG (b :: BackendType) v
  = AFColumn !(AnnColumnField b)
  ...
  | AFNodeId (XRelay b) !(TableName b) !(PrimaryKeyColumns b)
```

In this example, the code that deals with translating MSSQL's IR does not need to handle the `AFNodeId` constructor, for which there would be no corresponding implementation, since that constructor can never be constructed due to `XRelay` being `Void`.

### BackendMetadata

TODO: detail how we use it to create a `SourceInfo b`.

### BackendSchema

This typeclass is responsible for the implementation of the graphql parsers. We are keeping the same parser combinators approach that was already in use: ultimately, `GraphQL/Schema` will only contain generic components, such as `selectTable`, `tableSelectionSet`, or `functionArguments`, that call to the typeclass for backend-specific combinators, such as `parseColumn`. See [PDV documentation](https://github.com/hasura/graphql-engine/pull/4111) for how we write our schema code using parser combinators. At time of writing, most components are already generic, with the notable exception of Actions and Relay.

### BackendExecute

This typeclass is responsible for query planning: given a list of _root fields_ (selection fields appearing at the top level of the query / mutation / subscription), generate an `ExecutionPlan`, by building the corresponding monadic actions. As of now, each root field maps to one `ExecutionStep`, but this will likely change as we start implementing a dataloader.

Additionally, `BackendExecute` is responsible for deciding in which monad each execution step must be executed; for instance, at time of writing, Postgres uses `Tracing.TraceT (LazyTxT QErr IO)`, while MSSQL simply uses `IO`. How to deal with that monad and how to lift the results back to the overarching monad is the responsibility of the next typeclass: `BackendTransport`.

### BackendTransport

Finally, `BackendTransport` is responsible for executing each step over the network. Each backend can decide what to trace, what to log, and how to deal with each given step.

## Implementation challenges

### Heterogeneous containers and existential qualification

A major issue with having all types decorated with the Backend type is that it makes it difficult to store information about different backends in the same containers: if a GraphQL query contains a select on a postgres table and a select on a MSSQL table, how do store the heterogeneous list of `QueryRootField b`? In the schema cache, we keep a hashmap from `SourceName` to `SourceInfo`; how do we keep this now that `SourceInfo` is parameterized by the backend type? The answer is [existential types](https://wiki.haskell.org/Existential_type): we introduce wrapper types such as:

```haskell
data BackendSourceInfo = forall b. Backend b => BackendSourceInfo (SourceInfo b)
```

More generally, we use existential types **at the boundaries between the different steps**. The output of the metadata step is a `SourceInfo b`, hidden in the existential `BackendSourceInfo`. The parsers are generated based on each `SourceInfo`, and output an existential `QueryRootField`... Each step deals with the different backend in isolation, with its own typeclass, and outputs an existential type that erases the backend type. This allows us to keep a unified pipeline dealing with several different backends at the same time.

### Breaking the dependency cycle

**HERE BE DRAGONS**

A problem we have with the backend typeclasses is that we end up with a circular dependency between the existential container and the typeclass that uses it in the next step. For instance: we would ideally want for `SourceInfo` to know about `BackendSchema`; this would allow for the following:

```haskell
data BackendSourceInfo = forall b. BackendSchema b => BackendSourceInfo (SourceInfo b)

buildSource
  :: (BackendSchema b, MonadSchema m)
  => SourceInfo b
  -> m SomeExistentialSchemaInfo
buildSource = ...

buildAllSources
  :: MonadSchema m
  => SourceCache
  -> m [SomeExistentialSehemaInfo]
buildAllSources cache = for cache $ \(BackendSourceInfo sourceInfo) -> buildSource sourceInfo
```

Here, we can directly call `buildSource` on the `sourceInfo`, since the `BackendSchema b` constraint was in the constructor. But alas, it is neither possible nor desirable to do this, for two reasons:
- `BackendSchema` already needs to know about `SourceInfo`: we need access to the source cache while building the schema, and that constraint is exposed in `MonadSchema`, that the combinators in `BackendSchema` rely on; this creates a cycle, which I don't believe is even possible to break (partly because I ran into a [GHC bug](https://gitlab.haskell.org/ghc/ghc/-/issues/1012));
- the code that generates a `SourceInfo` would also need to know about `BackendSchema`, and add the corresponding constraints throughout the metadata code; on top of mixing concerns, this would also lead to some hard-to-break cycles.

Instead, we opt for a different solution: those existential types only know about `Backend`. Each step is a "black box", and each part can be kept in isolation: no need to know about `BackendSchema` to implement `BackendMetadata`. While this sounds like a major benefit, this leaves us with a difficult question: how do we do the dispatch to the typeclass, when the constraint was not part of the constructor? How do we implement `buildAllSources` in the example above?

We simply have to explicitly unpack / repack, sadly. We use a GADT to represent some backend tags, and we use this to identify the actual backend:

```haskell
data BackendTag (b :: BackendType) where
  PostgresTag :: BackendTag 'Postgres
  MSSQLTag    :: BackendTag 'MSSQL
  ...

data BackendSourceInfo = forall b. Backend b => BackendSourceInfo (BackendTag b) (SourceInfo b)

buildSource
  :: (BackendSchema b, MonadSchema m)
  => SourceInfo b
  -> m SomeExistentialSchemaInfo
buildSource = ...

buildAllSources
  :: MonadSchema m
  => SourceCache
  -> m [SomeExistentialSchemaInfo]
buildAllSources cache = for cache $ \(BackendSourceInfo backendTag sourceInfo) -> case backendTag of
  PostgresTag -> buildSource sourceInfo
  MSSQLTag    -> buildSource sourceInfo
  ...
```

As long as there is an instance of `BackendSchema` available in that scope, this works, even if the constraint wasn't part of the constructor. The major drawback is of course that we will now have a few explicit switch cases like this in the code base, at the boundary of each step... However, there is a solution for this as well: all of them will have the exact same shape: given an existential type, switch over the tag, and call a given function on each value. We could therefore use TemplateHaskell to generate those switch cases for us, to make sure that whenever a new backend is added to the `BackendType` enum, all dispatch functions are generated correctly. This is an optional and debatable quality of life option, that trades more code complexity in exchange for fewer changes to add a new backend.

## Drawbacks, alternatives, open questions, and future work

There are several potential failure modes with this approach, beyond the risk of over-engineering.

### Too big Backend class

At time of writing, `Backend` contains a plethora of types, and is quite verbose. This is the result of a short deadline for the first iteration of this project, which led to a pretty aggressive generalization of the codebase: easier to put anything "suspicious" in `Backend`, and we'll clean it later. As we refine the code, we will probably want to aggressively do the opposite: find a common representation for most types, that works across backends, and reduce the number of types.

### Cross-source interactions

Likewise, at a later stage, in future work, we will need to support operations that operate at a higher-level, across sources, such as client-side joins. We will either need to decide on a generic implementation of *table names* that is the same across all backends, or add more existential types... This will be a project of its own, but here again the same trade-off will apply: having types in `Backend` will allow for more granular implementation per backend (allowing us to properly handle things such as case sensitivity, or the notion of a "public schema") at the cost of more existentially qualified types; having generic names will be tricky wrt. some backends, but would allow for "normal" haskell.

### Growing IR

The more backends we add, the more we might need a fine control over every aspect of the IR: we might want to be able to prune every single feature with an extension type... This could lead, again, to an explosion of types in `Backend`, and an increase in complexity of the IR itself. A potential tradeoff here would be to accept that some features will be representable in the IR despite not being supported by all backends, making the translation code later on partial. This might be an acceptable trade-off if we have a good enough test suite.

### Test suite

We will need to investigate how to run our existing integration test suite on other backends. For now, the metadata setup by executing raw SQL on Postgres, which will no longer work. A solution could be to use an abstract representation of the setup, and maintain a connector per backend that sets up a test database based on this abstract representation?
