---
title: "useQuery hook"
metaTitle: "Apollo useQuery React hook | GraphQL React Apollo Hooks Tutorial"
metaDescription: "We will use the Apollo Client useQuery React hook from @apollo/react-hooks to make GraphQL queries"
---

import GithubLink from "../../src/GithubLink.js";

In this section, we will implement GraphQL Queries and integrate with the react UI.
With Apollo Client, you can send queries in 2 different ways.

1. Using the `query` method directly and then process the response.
2. New `useQuery` React hook with React. (Recommended)

### Apollo useQuery React Hook
The recommended method is to use the `useQuery` React hook, where you will just pass your GraphQL query and `useQuery` React hook will fetch the data automatically.

Great! Now let's define the graphql query to be used:

Open `src/components/Todo/TodoPrivateList.js` and add the following code:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/react-apollo-hooks/app-final/src/components/Todo/TodoPrivateList.js" text="src/components/Todo/TodoPrivateList.js" />

```javascript
import React, { useState, Fragment } from "react";
+ import gql from 'graphql-tag';

import TodoItem from "./TodoItem";
import TodoFilters from "./TodoFilters";

+ const GET_MY_TODOS = gql`
+  query getMyTodos {
+    todos(where: { is_public: { _eq: false} }, order_by: { created_at: desc }) {
+      id
+      title
+      created_at
+      is_completed
+  }
+ }`;
```

We have now written the graphql query as a javascript constant using the `gql` parser function. This function is used to parse the plain string as a graphql query.

What does this query do? 
------------------------
The query fetches `todos` with a simple condition; `is_public` must be false. We sort the todos descending by its `created_at` time according to the schema. We specify which fields we need for the todos node.

The query is now ready, let's integrate it with our react code.

```javascript

+ import {useQuery} from '@apollo/react-hooks';

```

`useQuery` React hook is being imported from `@apollo/react-hooks`

```javascript

import React, { Component, Fragment } from "react";
import {useQuery} from '@apollo/react-hooks'
import gql from 'graphql-tag';

import TodoItem from "./TodoItem";
import TodoFilters from "./TodoFilters";

const GET_MY_TODOS = gql`
  query getMyTodos {
    todos(where: { is_public: { _eq: false} }, order_by: { created_at: desc }) {
      id
      title
      created_at
      is_completed
  }
}`;

const TodoPrivateList = props => {
  ...
}

+ const TodoPrivateListQuery = () => {
+   const { loading, error, data } = useQuery(GET_MY_TODOS);
+ 
+   if (loading) {
+     return <div>Loading...</div>;
+   }
+   if (error) {
+     console.error(error);
+     return <div>Error!</div>;
+   }
+   return <TodoPrivateList todos={data.todos} />;
+ };

export default TodoPrivateList;
```

Remember that we wrapped our App component with `<ApolloProvider>` and passed `client` as a prop. `useQuery` React hook is using the same client. 

We are importing the `useQuery` React hook from `@apollo/react-hooks` and the graphql query we defined above to fetch the todo data.

Let's remove the mock `todos` data which was used to populate sample data.

```javascript

const TodoPrivateList = props => {
  const [state, setState] = useState({
    filter: "all",
    clearInProgress: false,
-    todos: [
-      {
-        id: "1",
-        title: "This is private todo 1",
-        is_completed: true,
-        is_public: false
-      },
-      {
-        id: "2",
-        title: "This is private todo 2",
-        is_completed: false,
-        is_public: false
-      }
-    ]
  });

  const filterResults = filter => {
    setState({
      ...state,
      filter: filter
    });
  };

  const clearCompleted = () => {};

-    let filteredTodos = state.todos;
+    const {todos} = this.props;
+
+    let filteredTodos = todos;
    if (state.filter === "active") {
-     filteredTodos = state.todos.filter(todo => todo.is_completed !== true);
+     filteredTodos = todos.filter(todo => todo.is_completed !== true);
    } else if (state.filter === "completed") {
-     filteredTodos = state.todos.filter(todo => todo.is_completed === true);
+     filteredTodos = todos.filter(todo => todo.is_completed === true);
    }

    const todoList = [];
    filteredTodos.forEach((todo, index) => {
      todoList.push(<TodoItem key={index} index={index} todo={todo} />);
    });

    return (
      ...
    );
  
};

```

Finally, update the exports.

```javascript
- export default TodoPrivateList;
+ export default TodoPrivateListQuery;
+ export {GET_MY_TODOS};
```

Woot! You have written your first GraphQL integration with React. Easy isn't it?

How does this work?
-------------------
When you use the `useQuery` React hook, Apollo returns the data along with other properties. Most important ones are:

`loading`: A boolean that indicates whether the request is in flight. If loading is true, then the request hasn't finished. Typically this information can be used to display a loading spinner.

`error`: A runtime error with graphQLErrors and networkError properties. Contains information about what went wrong with your query.

`data`: An object containing the result of your GraphQL query. This will contain our actual data from the server. In our case, it will be the todo data.

You can read more about other properties that result object contains [here](https://www.apollographql.com/docs/react/essentials/queries/#result)

Using the `data` property, we are parsing the results from the server. In our query, `data` property has an array `todos` which can be mapped over to render each `TodoItem`.

If you noted, there has been some client side filtering to the todos that are displayed.
