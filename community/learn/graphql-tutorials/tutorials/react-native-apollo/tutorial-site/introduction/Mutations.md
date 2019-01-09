# GraphQL mutations (writing data)

Like a GraphQL query, there are GraphQL mutations, they help you create/update/delete or generally cause some kind of modification of data in your backend.

This is an example of a GraphQL mutation that is inserting a todo:

```
mutation {
  insert_todo(objects:
    [{
      todo: "Learn GraphQL mutations",
      user_id: 1
    }]
  ) {
    returning {
      id
    }
  }
}
```

This GraphQL mutation is equivalent to a POST request that you might have made with a REST API:

```
POST /todo
{
  "todo": "Learn GraphQL mutations"
}

----

200 OK
{
  "id": 99,
}
```

## Key observations

1. The "arguments" describing the parameters of the mutation are sent in parentheses `()` next to the name of the mutation.
2. The content in the braces describes what you want fields you want in the response:

```
{
  returning {
    id
  }
}
```

## Make your first GraphQL mutation

1. Head to https://graphql-tutorials.com/graphiql
2. Insert a todo:

````
mutation {
  insert_todo(objects:
    [{
      todo: "Learn GraphQL mutations",
      user_id: 1
    }]
  ) {
    returning {
      id
    }
  }
}```
````
