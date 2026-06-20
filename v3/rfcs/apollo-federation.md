# Using Hasura in Apollo Federation

## Motivation

Apollo federation lets users combine multiple GraphQL APIs (called subgraphs) into a unified API. We want Hasura users to be able to use Hasura to build one of the subgraphs and bring it into Apollo federation. This requires us to provide a way in the Hasura metadata to be able to produce a GraphQL schema that can work with Apollo federation. 

### Apollo Federation

There are two aspects to integrating with Apollo federation:
- Being the left side of a relationship, where another Apollo subgraph will resolve the relationship
- Being the right side of a relationship, which Apollo will use for resolving relationships from other Apollo subgraphs

For the first case, if we want a relationship from a `Review` object to a `Product` object coming from Apollo, we need to generate a schema like this:

```graphql
type Review {
  id: Int!
  product: Product! # Note: in the database, this field would just be product_id
  description: String!
}

type Product @key(fields: "id", resolvable: false) {
  id: Int!
}
```

For the second case, if we want to allow a relationship from an  `Review` object in Apollo to a `Product` object in Hasura, we need to generate a schema like this:

```graphql
type Product @key(fields: "id") {
  id: Int!
  name: String!
}

query Query {
  _entities(representations: [_Any!]!): [_Entity]!
}

union _Entity = Product | ... # any other types that can be referenced from Apollo
```

## Proposed Metadata Changes

The following changes in the metadata will allow us to achieve both these use cases.

### Allow nesting NDC columns in OpenDD

First, we would need to allow mapping the columns of an NDC collection to a field nested within a sub-object within the OpenDD model's object type.

The current schema for `fieldMapping` in OpenDD will need to be changed to a oneOf to be able to support that.

So, for mapping an NDC collection for reviews with columns `id`, `description`, and `product_id`, to the desired GraphQL schema described in the [Apollo Federation section](#apollo-federation), the mapping would look like this:

```yaml
fieldMapping:
  id:
    column:
      name: id
  description:
    column:
      name: description
  product:
    nestedFieldMapping:
      id:
        column:
          name:
            product_id
```

### Allow generating Apollo directives

The following metadata can be configured to generate the appropriate Apollo directives.

For generating the directives for left side of a relationship.
```yaml
kind: ObjectType
version: v1
definition:
  name: Product
  fields:
    - name: id
      type: Int!
  graphql:
    typeName: Product
    apolloFederation:
      # generates the key directive for Product, with resolvable false if no model
      # is defined as the entitySource.
      keys:
        - fields:
          - id
```

For generating the types / directives for the right side of a relationship.
```yaml
kind: Model
version: v1
definition:
  name: Products
  objectType: Product
  graphql:
    apolloFederation:
        # generates the key directive on `Product` and adds it to `_Entity` union
        entitySource: true 
```
