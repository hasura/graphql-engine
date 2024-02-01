
# Introduction

This RFC proposes adding support to adding descriptions to open DD objects which will ultimately show up in the description of different types of entities in the GraphQL schema.

Description can be added to the following user facing types:

1. Scalars
2. Objects
3. Models
4. Commands
5. Relationships

Descriptions can be useful to the users of the GraphQL schema to understand a type/root field/argument better.


## Adding a description field to the metadata object


### `ScalarType` metadata object

The scalar type can only have one type of description associated with it, i.e.
the description of the GraphQL type.

```json
{
  "kind": "ScalarType",
  "version": "v1",
  "definition": {
    "name": "NonNegativeInt",
    "description": "Type to represent integers that are greater than or equal to 0",
    "graphql": {
      "typeName": "NonNegativeInt"
    }
  }
}
```

The description will show up in the GraphQL schema, as following:


```graphql
"""
Type to represent integers that are greater than or equal to 0
"""
scalar NonNegativeInt
```

### `ObjectType` metadata object

The `ObjectType` can have two kinds of descriptions:

1. Description of the object type.
2. Description of the individual fields.

```json
{
  "kind": "ObjectType",
  "version": "v1",
  "definition": {
    "name": "author",
    "description": "Author object containing unique identifier and name.",
    "fields": [
      {
        "name": "author_id",
        "type": "CustomInt!",
        "description": "ID to uniquely identify an author."
      },
      {
        "name": "first_name",
        "type": "String!"
      },
      {
        "name": "last_name",
        "type": "String!"
      }
    ],
    "globalIdFields": [
      "author_id"
    ],
    "graphql": {
      "typeName": "Author"
    }
  }
}
```

The description will show up in the GraphQL schema, as following:

```graphql
"""
Author object containing unique identifier and name.
"""
type Author {
  """
  ID to uniquely identify an author.
  """
  author_id: CustomInt!,
  first_name: String!,
  last_name: String!
}
```

### `Model` metadata object

A model can have three diffrent types of descriptions, the number of descriptions correspond
to the number of GraphQL APIs that are chosen to expose. At the moment of writing this
document, two types of GraphQL APIs are supported, `select_many` and `select_one`.

Models can also accept arguments and the argument fields can also accept descriptions.


```json
{
  "kind": "Model",
  "version": "v1",
  "definition": {
    "name": "Authors",
    "objectType": "author",
    "globalIdSource": true,
    "arguments": [
      {
        "name": "include_inactive",
        "type": "boolean",
        "description": "If set to true, returns authors even if they are inactive."
      }
    ],
    "graphql": {
      "selectUniques": [
        {
          "queryRootField": "AuthorByID",
          "description": "Returns at most an author identified by the given author_id, returns null if author is not found with the provided ID.",
          "uniqueIdentifier": [
            "author_id"
          ]
        }
      ],
      "selectMany": {
        "queryRootField": "AuthorMany",
        "description": "Selects multiple authors and supports filtering and pagination."
      },
      "filterExpressionType": "Author_Where_Exp",
      "orderByExpressionType": "Author_Order_By",
      "argumentsInputType": "Author_Arguments"
    },
    "filterableFields": [
      "author_id"
    ],
    "orderableFields": [
      "author_id"
    ]
  }
}
```

The descriptions will show up in the GraphQL schema, as following:

```graphql


type Query {
  """
  Selects multiple authors and supports filtering and pagination.
  """
  AuthorMany(
    args: Author_Arguments,
    where: Author_Where_Exp,
    limit: Int,
    offset: Int,
    order_by: Author_Order_By): [Author!]
  """
  Returns at most an author identified by the given author_id, returns null if author is not found with the provided ID.
  """
  AuthorByID(
    author_id: Int!,
    """If set to true, returns authors even if they are inactive."""
    include_inactive: boolean
    ): Author
}
```

### `Command` metadata object

Commands can have two kinds of descriptions. One is the description of the
root field that will be exposed by the command and second is the description
of the input arguments to the command.

```json
{
  "kind": "Command",
  "version": "v1",
  "definition": {
    "name": "get_article_by_id",
    "description": "Command to get an article by using the ID.",
    "arguments": [
      {
        "name": "article_id",
        "type": "Int!",
        "description": "ID of the article."
      }
    ],
    "outputType": "commandArticle",
    "graphql": {
      "rootFieldName": "getArticleById",
      "rootFieldKind": "Query"
    }
  }
}
```

The descriptions will show up in the GraphQL schema, as following:

```graphql
type Query {
  """
  Command to get an article by using the ID.
  """
  getArticleById(
    "ID of the article."
    article_id: Int!
  )
}
```

### `Relationship` metadata object

Relationships can have one kind of description. This description will become the
GraphQL description of the relationship field.


```json
{
  "kind": "Relationship",
  "version": "v1",
  "definition": {
    "source": "author",
    "name": "Articles",
    "description": "Fetches the corresponding articles of the author.",
    "target": {
        "model": {
          "name": "Articles",
          "relationshipType": "Array"
        }
      },
    "mapping": [
      {
        "source": {
          "fieldPath": [
            {
              "fieldName": "article_id"
            }
          ]
        },
        "target": {
          "modelField": [
            {
              "fieldName": "article_id"
            }
          ]
        }
      }
    ]
  }
}

```

The descriptions will show up in the GraphQL schema, as following:

```graphql
  type author {
    author_id: Int!,
    article_id: Int!,
    """
    Fetches the corresponding articles of the author.
    """
    Articles: [Articles!]
  }
```
