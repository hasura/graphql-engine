# V3 Contributing Guide

## Getting Started

### Using Docker

Start a development container (which includes NDC agents for testing):

```
docker compose run --build --rm dev_setup bash
```

### Without Docker

You will need to install some packages:

- The Rust compiler
- `protobuf-compiler`

For development, you may need to install some additional tools such as `nextest`. See the [Dockerfile](Dockerfile).

### Building the source

If the dependencies are correctly installed as above, you should now be able to run

```
cargo build
```

From here, you can follow the instructions in <README.md> to set up a working server.

## Design Principles

### Separation of concerns: Open DDS vs NDC

NDC (formerly known as GDC) was introduced in v2, primarily as a way to improve development speed of new backends, but it also had several ancillary benefits which can be attributed to the separation of concerns between the NDC agent and the API engine.

In v3, we will work exclusively against the NDC abstraction to access data in databases.

There will be a separation between Open DDS (the metadata which the user provides to describe the their data models and APIs), the v3 engine which implements the specification (as GDS), and NDC which provides access to data.

### Server should start reliably and instantly

A major problem in v2 was that the construction of a schema from the user’s metadata was slow, and could fail for several reasons (e.g. a database might have been unavailable). This meant that the server could fail to come back up after a restart, or replicas could end up with subtly different versions of the metadata.

In v3, the schema will be completely and uniquely determined by the Open DDS metadata.

NDC can be unavailable, or its schema differ from what is in the Open DDS metadata. These are fine, because the schema is determined only by the Open DDS metadata.

In fact, it is useful to allow these cases, because they will allow different deployment workflows in which the Open DDS metadata is updated before a database migration, for example.

### Open DDS: configuration over convention

In v2, there were several conventions baked into the construction of the schema from metadata.

E.g. all table root fields were named after the database table by default, or could be renamed after the fact in metadata. However, this meant that we had to prefix table names when we added new databases, in case their default names overlapped.

Several other type names and root field names have defaults in v2 metadata.

V3 adopts the principle that Open DDS metadata will be explicit about everything needed to determine the schema, so that no overlaps can occur if the data connector schema changes.

Open DDS metadata in general will favor configuration over convention everywhere, and any conventions that we want to add to improve the user experience should be implemented in the CLI or console instead.

## Server development guide

The most important parts of the code from a server point of view are illustrated here. Explanations of each are given below:

```
crates
├── engine
│   ├── bin
│   │   ├── engine
│   ├── src
│   │   ├── execute
│   │   ├── metadata
│   │   ├── schema
│   │   │   ├── operations
│   │   │   ├── types
├── lang-graphql
│   ├── src
│   │   ├── ast
│   │   ├── introspection
│   │   ├── lexer
│   │   ├── normalized_ast
│   │   ├── parser
│   │   ├── schema
│   │   ├── validation
├── open-dds
```

### `engine/bin/engine`

This executable takes in a metadata file and starts the v3 engine according to that file.

### `open-dds`

This crate contains the Open DDS metadata structure and an accessor library. This metadata is used to start the v3 engine.

### `engine`

This crate implements the Open DDS specification on top of the GraphQL primitives provided by the `lang-graphql` crate. It is responsible for validating Open DDS metadata, creating a GraphQL schema from resolved Open DDS metadata, and implementing the GraphQL operations.

#### `engine/src/metadata`

Resolves/validates the input Open DDS metadata and creates intermedaiate structures that are using in the rest of the crate for schema generation.

#### `engine/src/schema`

Provides functions to resolve the Open DDS metadata, generate the GraphQL scehma from it, and execute queries against the schema.

#### `engine/src/schema/operations`

Contains the logic to define and execute the operations that would be defined by the Open DDS spec.

Technically, these are fields of the `query_root`/`subscription_root` or `mutation_root` and as such can be defined in `schema::types::*_root` module. However, this separation makes it easier to organize them (for example `subscription_root` can also import the same set of operations).

Each module under `operations` would roughly define the following:

- IR: To capture the specified operation.
- Logic to generate schema for the given operation using data from resolved metadata.
- Logic to parse a normalized field from the request into the defined IR format.
- Logic to execute the operation.

#### `engine/src/schema/types`

TODO: This is a bit outdated, so we should fix this.

Contains one module for each GraphQL type that we generate in the schema. For example:

- `model_selection`: An object type for selecting fields from a table (eg: type of the table_by_pk field in query_root).
- `query_root`: An object type that represents the entry point for all queries.

Each module under `types` defines the following:

- IR: A container for a value of the type.
- Logic to generate schema for the given type using data from resolved metadata.
- Logic to parse a normalized object (selection set or input value) from the request into the defined IR format.

### `lang-graphql`

This crate is an implementation of the GraphQL specification in Rust. It provides types for the GraphQL AST, implements the lexer and parser, as well as validation and introspection operations.

#### `lang-graphql/src/ast`

The raw GraphQL AST (abstract syntax tree) types that are emitted by the parser.

#### `lang-graphql/src/introspection`

Provides schema and type introspection for GraphQL schemas.

#### `lang-graphql/src/lexer`

Lexer that emits tokens (eg: String, Number, Punctuation) for a raw GraphQL document string.

#### `lang-graphql/src/normalized_ast`

The normalized AST types. The raw AST can be validated and elaborated with respect to a GraphQL schema, producing the normalized AST. 

#### `lang-graphql/src/parser`

Parser for GraphQL documents (executable operations and schema documents).

#### `lang-graphql/src/schema`

Types to define a GraphQL schema.

#### `lang-graphql/src/validation`

Validates GraphQL requests vs a schema, and produces normalized ASTs, which contain additional relevant data from the schema. 
