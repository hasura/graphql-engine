---
title: "Query Variables"
metaTitle: "Query Variables | GraphQL ReasonML React Apollo Tutorial"
metaDescription: "GraphQL Mutation to create new personal todos by parameterizing with query variables."
---

import GithubLink from "../../src/GithubLink.js";

What is a variable in GraphQL context?
-------------------------------------
GraphQL has a first-class way to factor dynamic values out of the query, and pass them as a separate dictionary. These values are called variables. In our case, we are defining the object to be inserted as a mutation.

So let's define the graphql mutation to be used.

Open `src/GraphQLQueries.re` and add the following code:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/reason-react-apollo/app-final/src/GraphQLQueries.re" text="GraphQLQueries.re" />

```javascript
// GraphQL mutation for inserting a todo
module InsertMyTodo = [%graphql
  {|
    mutation ($todo: String!, $isPublic: Boolean!) {
      insert_todos(objects: {title: $todo, is_public: $isPublic}) {
        affected_rows
        returning {
          id
          title
          created_at
          is_completed
        }
      }
    }
  |}
];
module InsertMyTodoMutation = ReasonApollo.CreateMutation(InsertMyTodo);

```

In the above code,

- `InsertMyTodo` is a mutation module built from plain query string using `graphql_ppx`. 
- `InsertMyTodoMutation` is a typed React component that provides the `mutation` function (a function that performs the given mutation) in its render prop function so that it can be utilised by the events. It is similar to the `<Mutation>` component in `react-apollo`.


What does this mutation do?
---------------------------
The mutation inserts into `todos` table with the `objects` variable being passed as one todo type.

Awesome! We have defined our first graphql mutation. In the next section, we will pass the query variables to this mutation and finally integrate this mutation with our `insert todo` functionality.
