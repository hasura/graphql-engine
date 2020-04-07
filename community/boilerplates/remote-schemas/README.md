# Remote Schema (GraphQL Server) Boilerplates

Hasura GraphQL Engine can combine schemas from multiple remote GraphQL servers
and expose them at a single endpoint. You can write these GraphQL servers in any
language and Hasura takes care of stitching together the schema from these
servers ([read more](../../../remote-schemas.md)).

This directory contains boilerplates for GraphQL servers using various
languages, frameworks and deployment runtimes:

- nodejs: [aws](aws-lambda/nodejs), [azure](azure-functions/nodejs), [google](google-cloud-functions/nodejs)
- python: [zeit](zeit-now/python)
- [rust](https://github.com/ronanyeah/rust-hasura)

Typical use-cases:

- Wrap existing REST endpoints: [rest-wrapper](rest-wrapper/)

## Docs

[Remote Schema documentation](https://hasura.io/docs/1.0/graphql/manual/remote-schemas/index.html)

## Architecture

![Remote schema architecture diagram](../../../assets/remote-schemas-arch.png)
