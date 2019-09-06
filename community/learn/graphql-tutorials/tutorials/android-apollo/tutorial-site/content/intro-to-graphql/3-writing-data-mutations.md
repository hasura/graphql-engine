---
title: Writing data - Mutations
metaTitle: "GraphQL Mutations to insert data | GraphQL Android Apollo Tutorial"
metaDescription: "Try out GraphQL Mutation using GraphiQL. A GraphQL mutation example with dynamic arguments and variables to insert data"
---

import {Link} from 'gatsby';

These are the concepts you should know before you attack mutations (haha):
- <Link to="/intro-to-graphql/2-fetching-data-queries#graphiql">Using GraphiQL</Link>
- <Link to="/intro-to-graphql/2-fetching-data-queries#query-variables">Using query variables</Link>

Now, let's get started with seeing how we can use GraphQL to "write" data.
GraphQL mutations are types of GraphQL queries that may result in the state
of your backend "mutating" or changing, just like typical `'POST'`,
`'PUT'`, `'PATCH'`, `'DELETE'` APIs.

## Basic mutations
Since we're using Hasura for our GraphQL API, we get mutations for
insert, updates or deletes that we can use in our app.

Let's try these mutations out in the context of a todo app to see
what mutations look like. Mutations that you get from another GraphQL
service, say if your API team has built their own,  might be different.

### Create a todo

Let's make an API call to create a todo. As you would have guessed, this
will be a critical portion of our todo app. 😉

> **Protip**: Now let's say we don't know what the name of the mutation to 
> create a todo. GraphiQL to the rescue!
> Head to GraphiQL and on the right, click on the "docs" tab.
> Type "todo" there and you'll see a list of GraphQL queries and types
> that use todo. Read through their descriptions and you'll soon
> find that `insert_todo` is what you need.

The mutation to create todos is titled `insert_todos`.

```graphql
mutation {
  insert_todos(objects: [{title: "new todo"}]) {
    returning {
      id
    }
  }
}
```

<!-- [//]: # TODO: -->
<b><a href="https://learn.hasura.io/graphql/graphiql" target="_blank">Try it out in GraphiQL</a></b>

## Returning data after the mutation
Notice that the data of the todo that is to be inserted is sent as
an argument to the `insert_todos` mutation. But the "fields" of the mutation
specify the shape of the _response_ that you want from the server.

Let's say we'd like to get the entire todo object once it's been created
as a response:

```graphql
mutation {
  insert_todos(objects: [{title: "new todo"}]) {
    returning {
      id
      title
      is_completed
      is_public
      created_at
    }
  }
}
```

<!-- [//]: # TODO: -->
<b><a href="https://learn.hasura.io/graphql/graphiql" target="_blank">Try it out in GraphiQL</a></b>

## Parametrise what you insert

For mutations, we would almost always have to paramatrise the arguments! We
would rarely, if ever, have a "hardcoded" mutation in our app. This is because
the arguments of what data to capture, how to modify or delete something is usually
dependent on some user action.

Now that we know how to parametrise using query variables, let's use that:

```graphql
# The parametrised GraphQL mutation
mutation($todo: todos_insert_input!){
  insert_todos(objects: [$todo]) {
    returning {
      id
    }
  }
}
```

```javascript
# As a query variable
{
  "todo": {
    "title": "A new dynamic todo"
  }
}
```

<!-- [//]: # TODO: -->
<b><a href="https://learn.hasura.io/graphql/graphiql" target="_blank">Try it out in GraphiQL</a></b>

We'll explore more mutations to update or delete data a little later.
This is a good start to grokking mutations!

## Summary

- You can make basic GraphQL mutations
- You can pass dynamic arguments/data to mutations with query variables

Next, let's look at GraphQL subscriptions
