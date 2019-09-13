---
title: "useQuery Hook"
metaTitle: "Apollo Query with useQuery Hook | GraphQL React Apollo Typescript Tutorial"
metaDescription: "We will use the Apollo useQuery Hook from @apollo/react-hooks. This hook is used to fetch data and return data, loading and error"
---

import GithubLink from "../../src/GithubLink.js";

In this section, we will implement GraphQL Queries and integrate with the react UI.
With Apollo Client, you can send queries in 3 different ways.

1. Using the `client.query` method directly and then process the response.
2. Render Prop API with React.
3. Using the `useQuery` hook. (Recommended)

### Apollo useQuery Hook
The recommended method is to use the `useQuery` hook, where you will just wrap your GraphQL query to the `gql` function and will fetch the data automatically and will give a result object containing data, loading and error properties.

Great! Now let's define the graphql query to be used:

Open `src/components/Todo/TodoPrivateList.tsx` and add the following code:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/typescript-react-apollo/app-final/src/components/Todo/TodoPrivateList.tsx" text="src/components/Todo/TodoPrivateList.tsx" />

```javascript
  import React, { Fragment, useState } from "react";
+ import gql from 'graphql-tag';
  import TodoItem from "./TodoItem";
  import TodoFilters from "./TodoFilters";

+ const GET_MY_TODOS = gql`
+  query getMyTodos {
+    todos(where: { is_public: { _eq: false} }, order_by: { created_at: desc }) {
+      id
+      title
+      is_completed
+  }
+ }`;

  type Todo = {
    id: number,
    title: string,
    is_completed: boolean
  };

```

We have now written the graphql query as a javascript constant using the `gql` parser function. This function is used to parse the plain string as a graphql query.

The query is now ready, let's integrate it with our react code.

```javascript
  import React, { Fragment, useState } from "react";
  import gql from 'graphql-tag';
+ import { useQuery } from "@apollo/react-hooks";

```

`useQuery` hook is being imported from `@apollo/react-hooks`

```javascript
  import React, { Fragment, useState } from "react";
  import gql from "graphql-tag";
  import { useQuery } from "@apollo/react-hooks";
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
    }
  `;

  type Todo = {
    id: number,
    title: string,
    is_completed: boolean,
    is_public: boolean
  };

  const TodoPrivateList = () => {

    const [filter, setFilter] = useState<string>("all");
+   const { loading, error, data } = useQuery(GET_MY_TODOS);
    ...
    ...
    const clearCompleted = () => {
    };
    
+   if(loading) {
+     return (<div>Loading...</div>);
+   }
+   if(error || !data) {
+     return (<div>Error...</div>);
+   }

-   let filteredTodos = todos;
+   let filteredTodos = data.todos;
    if (filter === "active") {
-     filteredTodos = todos.filter((todo: Todo) => todo.is_completed !== true);
+     filteredTodos = data.todos.filter((todo: Todo) => todo.is_completed !== true);
    } else if (filter === "completed") {
-     filteredTodos = todos.filter((todo: Todo) => todo.is_completed === true);
+     filteredTodos = data.todos.filter((todo: Todo) => todo.is_completed === true);
    }

    ...
    ...

    return (
      ...
    );
  }

  export default TodoPrivateList;
  export { GET_MY_TODOS };

```

With the `useQuery` hook from `@apollo/react-hooks` and the graphql query we defined above, we fetch the todo data.

Let's remove the mock `todos` data which was used to populate sample data.

```javascript
  const TodoPrivateList = () => {

    const [filter, setFilter] = useState<string>("all");
    const { loading, error, data } = useQuery(GET_MY_TODOS);

-   const todos = [
-     {
-       id: 1,
-       title: "This is private todo 1",
-       is_completed: true
-     },
-     {
-       id: 2,
-       title: "This is private todo 2",
-       is_completed: false
-     }
-   ];

    const filterResults = (filter: string): void => {
      setFilter(filter);
    };
    ...

```

Woot! You have written your first GraphQL integration with React and Typescript. Easy isn't it?

How does this work?
-------------------

The `useQuery` hook returned a result object containing the following:

`loading`: A boolean that indicates whether the request is in flight. If loading is true, then the request hasn't finished. Typically this information can be used to display a loading spinner.

`error`: A runtime error with graphQLErrors and networkError properties. Contains information about what went wrong with your query.

`data`: An object containing the result of your GraphQL query. This will contain our actual data from the server. In our case, it will be the todo data.

You can read more about other values that Apollo returns in the result object [here](https://www.apollographql.com/docs/react/essentials/queries/#result)

Using the `data` prop, we are parsing the results from the server. In our query, `data` prop has an array of `todos` which can be mapped over to render each `TodoItem`.

If you noted, there has been some client side filtering to the todos that are displayed.
