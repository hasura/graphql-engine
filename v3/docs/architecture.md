# Architecture

## Project structure

The most important parts of the code from a server point of view are illustrated
here. Explanations of each are given below:

```
crates
├── open-dds
├── metadata-resolve
├── graphql
│   ├── lang-graphql
│   │   ├── src
│   │   │   ├── ast
│   │   │   ├── normalized_ast
│   │   │   ├── lexer
│   │   │   ├── parser
│   │   │   ├── schema
│   │   │   ├── introspection
│   │   │   ├── validation
│   ├── schema
│   │   ├── operations
│   │   ├── types
│   ├── ir
│   ├── frontend
├── execute
├── jsonapi
├── sql
├── engine
│   ├── bin
│   │   ├── engine
```

### `open-dds`

This crate contains the Open DDS metadata structure and an accessor library.
This metadata is used to specify the data models, permissions, connectors, and
essentially everything the engine needs to know about the project when it
starts.

### `graphql/lang-graphql`

This crate is an implementation of the GraphQL specification in Rust. It
provides types for the GraphQL AST, implements the lexer and parser, as well as
validation and introspection operations.

#### `graphql/lang-graphql/src/ast`

The raw GraphQL AST (abstract syntax tree) types that are emitted by the parser.

#### `graphql/lang-graphql/src/normalized_ast`

The normalized AST types. The raw AST can be validated and elaborated with
respect to a GraphQL schema, producing the normalized AST.

#### `graphql/lang-graphql/src/lexer`

Lexer that emits tokens (eg: String, Number, Punctuation) for a raw GraphQL
document string.

#### `graphql/lang-graphql/src/parser`

Parser for GraphQL documents (executable operations and schema documents).

#### `graphql/lang-graphql/src/schema`

Types to define a GraphQL schema.

#### `graphql/lang-graphql/src/introspection`

Provides schema and type introspection for GraphQL schemas.

#### `graphql/lang-graphql/src/validation`

Validates GraphQL requests vs a schema, and produces normalized ASTs, which
contain additional relevant data from the schema.

##### `metadata-resolve`

Resolves and validates the input Open DDS metadata and creates intermediate
structures that are used in the `engine` crate for schema generation.

##### `graphql/schema`

Provides functions to resolve the Open DDS metadata, generate the GraphQL scehma
from it, and execute queries against the schema.

##### `graphql/schema/operations`

Contains the logic to define and execute the operations that would be defined by
the Open DDS spec.

Technically, these are fields of the `query_root`, `subscription_root` or
`mutation_root` and as such can be defined in `graphql_schema::types::*_root`
module. However, this separation makes it easier to organize them (for example
`subscription_root` can also import the same set of operations).

Each module under `operations` would roughly define the following:

- IR: To capture the specified operation.
- Logic to generate schema for the given operation using data from resolved
  metadata.
- Logic to parse a normalized field from the request into the defined IR format.
- Logic to execute the operation.

##### `graphql/schema/types`

TODO: This is a bit outdated, so we should fix this.

Contains one module for each GraphQL type that we generate in the schema. For
example:

- `model_selection`: An object type for selecting fields from a table (eg: type
  of the table_by_pk field in query_root).
- `query_root`: An object type that represents the entry point for all queries.

Each module under `types` defines the following:

- IR: A container for a value of the type.
- Logic to generate schema for the given type using data from resolved metadata.
- Logic to parse a normalized object (selection set or input value) from the
  request into the defined IR format.

### `graphql/ir`

Responsible for combining the user input and our resolved metadata into our
intermediate representation ready to plan a request.

### `execute`

Responsible for the core operation of the engine in the context of a user
provided metadata, including requests processing, executing requests, etc.

### `graphql/frontend`

Entrypoints for GraphQL requests. Orchestrates parsing, validation and planning
requests.

### `sql`

Responsible for SQL frontend currently in development

### `jsonapi`

Responsible for JSONAPI frontend currently in development

#### `engine/bin`

Entry point to the program. The executable takes in a metadata file and starts
the v3 engine according to that file.

## Design Principles

### Separation of concerns: Open DDS vs NDC

NDC (formerly known as GDC) was introduced in v2, primarily as a way to improve
development speed of new backends, but it also had several ancillary benefits
which can be attributed to the separation of concerns between the NDC agent and
the API engine.

In v3, we will work exclusively against the NDC abstraction to access data in
databases.

There will be a separation between Open DDS (the metadata which the user
provides to describe the their data models and APIs), the v3 engine which
implements the specification (as GDS), and NDC which provides access to data.

### Server should start reliably and instantly

A major problem in v2 was that the construction of a schema from the user’s
metadata was slow, and could fail for several reasons (e.g. a database might
have been unavailable). This meant that the server could fail to come back up
after a restart, or replicas could end up with subtly different versions of the
metadata.

In v3, the schema will be completely and uniquely determined by the Open DDS
metadata.

NDC can be unavailable, or its schema differ from what is in the Open DDS
metadata. These are fine, because the schema is determined only by the Open DDS
metadata.

In fact, it is useful to allow these cases, because they will allow different
deployment workflows in which the Open DDS metadata is updated before a database
migration, for example.

### Open DDS: configuration over convention

In v2, there were several conventions baked into the construction of the schema
from metadata.

E.g. all table root fields were named after the database table by default, or
could be renamed after the fact in metadata. However, this meant that we had to
prefix table names when we added new databases, in case their default names
overlapped.

Several other type names and root field names have defaults in v2 metadata.

V3 adopts the principle that Open DDS metadata will be explicit about everything
needed to determine the schema, so that no overlaps can occur if the data
connector schema changes.

Open DDS metadata in general will favor configuration over convention
everywhere, and any conventions that we want to add to improve the user
experience should be implemented in the CLI or console instead.
