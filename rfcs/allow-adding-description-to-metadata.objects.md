# Allow adding descriptions to metadata objects in Hasura v3

Currently the description field in the GraphQl schema is `null` be default for custom metadata objects such as `models`, `commands`, etc. This should be customisable to add desctions for a metadata type. 

Descriptions can be an optional field added in the metadata subgraph which will be added to the GraphQl schema. Description can be added to the following types:

1. Scalars
2. Objects
    1. Top level object description
    2. Object fields
3. Models
    1. Arguments
    2. select_unique type
    3. select_many type
4. Commands
    1. Top level command description
    2. Arguments
5. Relationships

# How the metadata objects will look

## 1. Scalars

### Metadata object

User should be able to add a description to a custom scalar as follows:

```json
{
  "kind": "ScalarType",
  "version": "v1",
  "definition": {
    "name": "CustomInt",
  "description": "customInt description",
  "graphql": {
      "typeName": "CustomInt"
    }
  }
}
```

### Generated schema type

```json
{
  "data": {
    "__schema": {
      "queryType": {
        "name": "Query"
      },
      "mutationType": {
        "name": "Mutation"
      },
      "subscriptionType": null,
      "types": [
        {
          "kind": "SCALAR",
          "name": "CustomInt",
          "description": "customInt description",
          "fields": null,
          "inputFields": null,
          "interfaces": null,
          "enumValues": null,
          "possibleTypes": null
        }
      ],
      "directives": []
    }
  }
}
```

## 2. Objects

Objects can have descriptions for the type as well as for each of the fields in the objects.

### Metadata object

```json
{
  "kind": "ObjectType",
  "version": "v1",
  "definition": {
    "name": "author",
    "description": "author description",
    "fields": [
      {
        "name": "author_id",
        "type": "CustomInt!",
        "description": "author_id description"
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

### Generated schema type

```json
{
  "data": {
    "__schema": {
      "types": [
        {
          "kind": "OBJECT",
          "name": "Author",
          "description": "author description",
          "fields": [
            {
              "name": "author_id",
              "description": "author_id description",
              "args": [],
              "type": {
                "kind": "NON_NULL",
                "name": null,
                "ofType": {
                  "kind": "SCALAR",
                  "name": "CustomInt",
                  "ofType": null
                }
              },
              "isDeprecated": false,
              "deprecationReason": null
            },
            {
              "name": "first_name",
              "description": null,
              "args": [],
              "type": {
                "kind": "NON_NULL",
                "name": null,
                "ofType": {
                  "kind": "SCALAR",
                  "name": "String",
                  "ofType": null
                }
              },
              "isDeprecated": false,
              "deprecationReason": null
            }
          ]
        }
      ]
    }
  }
}
```

## 3. Models

Models have select unique as well as select many types. Users should be able to add descriptions  to them. We can also add description to the arguments for select_unique types

### Example 1

#### Metadata object without arguments

```json
{
  "kind": "Model",
  "version": "v1",
  "definition": {
    "name": "Authors",
    "objectType": "author",
    "globalIdSource": true,
    "source": {"..."},
    "graphql": {
      "selectUniques": [
        {
          "queryRootField": "AuthorByID",
          "description": "AuthorByID description",
          "uniqueIdentifier": [
            "author_id"
          ]
        }
      ],
      "selectMany": {
        "queryRootField": "AuthorMany",
        "description": "description"
      },
      "filterExpressionType": "Author_Where_Exp",
      "orderByExpressionType": "Author_Order_By"
    },
    "filterableFields": ["..."],
    "orderableFields": ["..."]
  }
}
```

#### Generated schema type

##### select_unique

```json
{
  "data": {
    "__schema": {
      "types": [
        {
          "name": "AuthorByID",
          "description": "AuthorByID description",
          "args": [
            {
              "name": "author_id",
              "type": {"..."},
              "defaultValue": null
            }
          ],
          "type": {"..."},
          "isDeprecated": false,
          "deprecationReason": null
        }
      ]
    }
  }
}
```
##### select_many schema object:

```json
{
  "data": {
    "__schema": {
      "types": [
        {
          "name": "AuthorMany",
          "description": "AuthorMany desc",
          "args": [
            {
              "name": "limit",
              "description": null,
              "..."
            },
            "..."
          ],
          "type": {"..."},
          "isDeprecated": false,
          "deprecationReason": null
        }
      ]
    }
  }
}
```

### Example 2

#### Metadata object with arguments

```json
{
  "kind": "Model",
  "version": "v1",
  "definition": {
    "name": "ArticlesByAuthor",
    "objectType": "article",
    "arguments": [
      {
        "name": "author_id",
        "type": "Int!",
        "description": "ArticlesByAuthor argument description"
      }
    ],
    "source": {"..."},
    "graphql": {
      "selectUniques": [
        {
          "queryRootField": "ArticlesByAuthorByID",
          "uniqueIdentifier": [
            "article_id"
          ],
          "description": "ArticlesByAuthorByID description"
        }
      ],
      "selectMany": {
        "queryRootField": "ArticlesByAuthorMany",
        "description": "ArticlesByAuthorMany description"
      },
      "orderByExpressionType": "ArticlesByAuthorOrderBy",
      "filterExpressionType": "ArticlesByAuthorWhereExp",
      "argumentsInputType": "ArticlesByAuthorArgs"
    },
    "filterableFields": ["..."],
    "orderableFields": ["..."]
  }
}
```

#### Generated Schema

##### select_unique

```json
{
  "data": {
    "__schema": {
      "types": [
        {
          "name": "ArticlesByAuthorByID",
          "description": "ArticlesByAuthorByID description",
          "args": [
            {
              "name": "article_id",
              "description": null,
              "..."
            },
            {
              "name": "author_id",
              "description": "ArticlesByAuthor argument description",
              "type": {
                "kind": "NON_NULL",
                "name": null,
                "..."
              }
            }
          ],
          "type": {"..."},
          "isDeprecated": false,
          "deprecationReason": null
        }
      ]
    }
  }
}
```

##### select_many

```json
{
  "data": {
    "__schema": {
      "types": [
        {
          "name": "ArticlesByAuthorMany",
          "description": "ArticlesByAuthorMany description",
          "args": [
            {
              "name": "args",
              "description": null,
              "..."
            }
            "..."
          ],
          "type": {"..."},
          "isDeprecated": false,
          "deprecationReason": null
        }
      ]
    }
  }
}
```

### Example 3

#### Metadata object with top level model description

If the description for select uniques/many configuration of the model is not present and model description is present,
then the model description will be used for the respective configuration.

```json
{
  "kind": "Model",
  "version": "v1",
  "definition": {
    "name": "Authors",
    "description": "top level model description",
    "objectType": "author",
    "globalIdSource": true,
    "source": {"..."},
    "graphql": {
      "selectUniques": [
        {
          "queryRootField": "AuthorByID",
          "description": "AuthorByID description",
          "uniqueIdentifier": [
            "author_id"
          ]
        }
      ],
      "selectMany": {
        "queryRootField": "AuthorMany",
      },
      "filterExpressionType": "Author_Where_Exp",
      "orderByExpressionType": "Author_Order_By"
    },
    "filterableFields": ["..."],
    "orderableFields": ["..."]
  }
}
```

#### Generated schema type

##### select_unique

```json
{
  "data": {
    "__schema": {
      "types": [
        {
          "name": "AuthorByID",
          "description": "AuthorByID description",
          "args": [
            {
              "name": "author_id",
              "type": {"..."},
              "defaultValue": null
            }
          ],
          "type": {"..."},
          "isDeprecated": false,
          "deprecationReason": null
        }
      ]
    }
  }
}
```
##### select_many schema object:

```json
{
  "data": {
    "__schema": {
      "types": [
        {
          "name": "AuthorMany",
          "description": "Selects multiple objects from the model. Model description: top level model description",
          "args": [
            {
              "name": "limit",
              "description": null,
              "..."
            },
            "..."
          ],
          "type": {"..."},
          "isDeprecated": false,
          "deprecationReason": null
        }
      ]
    }
  }
}
```

## 4. Commands

### Metadata object

```json
{
  "kind": "Command",
  "version": "v1",
  "definition": {
    "name": "get_article_by_id",
    "description": "command description",
    "arguments": [
      {
        "name": "article_id",
        "type": "Int!",
    "description": "article_id description"
      }
    ],
    "outputType": "commandArticle",
    "source": {"..."},
    "graphql": {
      "rootFieldName": "getArticleById",
      "rootFieldKind": "Query"
    }
  }
}
```

### Generated schema type

```json
{
  "data": {
    "__schema": {
      "types": [
        {
          "name": "getArticleById",
          "description": "get_article_by_id description",
          "args": [
            {
              "name": "article_id",
              "description": "article_id description",
              "type": {"..."},
              "defaultValue": null
            }
          ],
          "type": {"..."},
          "isDeprecated": false,
          "deprecationReason": null
        }
      ]
    }
  }
}
```

## 5. Relationships

Description can be added to the relationship name and will be shown as a comment wherever the relationship is being used in the schema

### Metadata object

```json
{
  "kind": "Relationship",
  "version": "v1",
  "definition": {
    "source": "author",
    "name": "Articles",
  "description": "description",
    "target": {
      "model": {
        "name": "Articles",
        "relationshipType": "Array"
      }
    },
    "mapping": ["..."]
  }
}
```

### Generated schema type

```json
{
  "data": {
    "__schema": {
      "types": [
        {
          "kind": "OBJECT",
          "name": "Author",
          "description": "author description",
          "fields": [
            {
              "name": "Articles",
              "description": "description",
              "args": ["..."],
              "type": {"..."},
              "isDeprecated": false,
              "deprecationReason": null
            }
          ]
        }
      ]
    }
  }
}
```
