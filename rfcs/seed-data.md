## How should we model the interface to add seed data to the database ?

### Motivation

There is an ongoing discussion in [#2431](https://github.com/hasura/graphql-engine/issues/2431) about how we should model an interface for adding seed data. Two approaches have come to light from the discussions which are described below.

#### Approach 1: Add a new "seed" command to CLI

In this approach, the user has to write the corresponding SQL for seeds and everything else taken care by the CLI. But here the user is limited to writing SQL. To explain this situation let's take the case of a person coming from a ruby background, they will be used to writing seed migrations in Ruby DSL and might find difficulty in adapting to the new change.

A PR [#3614](https://github.com/hasura/graphql-engine/pull/3614) has been submitted which implements this approach.

#### Approach 2: Delegate adding seed data completely to the user

A user can use whatever interface they want to communicate with the database, let it be GraphQL mutations or any ORM. From this [comment](https://github.com/hasura/graphql-engine/issues/2431#issuecomment-566033630) it is evident that atleast some users have this in mind.

In this approach, everything is left to the user, from connecting to the underlying database to writing and managing seeds.

An example for this approach would be Prisma. Prisma CLI has a seed command and they have a `prisma.yml` file which can be populated as follows.

```
seed:
  run: node ./data/seed.js
```

On running `prisma seed`, will in turn run `node ./data/seed.js`, The user is delegated the responsibility of managing seeds.

## Proposed change
From the discussions in the RFC the majority feels like we should go ahead with approach 1. 
This will introduce a new command `hasura seed` and two new subcommands `create` and `apply`. 

- `hasura seed create <name>` will create a new seed file and will open it in the default editor.
- `hasura seed apply` will try to apply all migrations files in the `seeds/` directory.

## Implementation

PR [#3614](https://github.com/hasura/graphql-engine/pull/3614) is very similar implementation of the proposed changes, but it is tied to the schema migrations implementation, which as per discussion around [comment1](https://github.com/hasura/graphql-engine/pull/3763#issuecomment-578011460) and [comment2](https://github.com/hasura/graphql-engine/pull/3763#issuecomment-578071739) feels like something to avoid.

I've a draft implementation (part of the RFC), which implements the proposed changes. Also adding an API endpoint which can probably be used by console (similar to migration endpoints).
