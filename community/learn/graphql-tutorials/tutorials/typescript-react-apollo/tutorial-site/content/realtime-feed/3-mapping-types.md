---
title: "Mapping Types"
metaTitle: " Mapping GraphQL Subscriptions to Typescript | GraphQL React Apollo Typescript Tutorial"
metaDescription: "We will see how to import the auto-generated Typescript types for GraphQL subscriptions to add type safety to Realtime component"
---

This is the final section of the tutorial. Here we will import type definitions relevant for this component.

### Types for fetching older todos

```javascript

  import React, { Fragment, useState, useRef, useEffect } from "react";
  import { useSubscription, useApolloClient } from "@apollo/react-hooks";
  import gql from "graphql-tag";
  import TaskItem from "./TaskItem";
+ import {
+   GetOldPublicTodosQuery,
+   GetOldPublicTodosQueryVariables,
+   Todos
+ } from '../../generated/graphql';

- type TodoItem = {
-   id: number,
-   title: string,
-   user: { name: string }
- }

+ type publicListProps = {
+   latestTodo?: Partial<Todos> | null
+ }

```

Now we will handle the case of latestTodo object being undefined.

```javascript

  const TodoPublicList = (props: publicListProps) => {
    const [olderTodosAvailable, setOlderTodosAvailable] = useState(props.latestTodo ? true : false)
    const [newTodosCount, setNewTodosCount] = useState(0)
    const [error, setError] = useState(false);
-   const [todos, setTodos] = useState<TodoItem[]>([]);
+   const [todos, setTodos] = useState<GetOldPublicTodosQuery["todos"]>([]);

-   let oldestTodoId = useRef(props.latestTodo ? props.latestTodo.id + 1 : 0);
+   let oldestTodoId = useRef(props.latestTodo && props.latestTodo.id ? props.latestTodo.id + 1 : 0);
-   let newestTodoId = useRef(props.latestTodo ? props.latestTodo.id : 0);
+   let newestTodoId = useRef(props.latestTodo && props.latestTodo.id ? props.latestTodo.id : 0);
    if(todos && todos.length) {
      oldestTodoId.current = todos[todos.length - 1].id
      newestTodoId.current = todos[0].id;
    }

```

Let's apply the type definitions to `client.query` in loadOlder function.

```javascript

-   const { data, networkStatus } = await client.query({
+   const { data, networkStatus } = await client.query<GetOldPublicTodosQuery, GetOldPublicTodosQueryVariables>({
      query: GET_OLD_PUBLIC_TODOS,
      variables: { oldestTodoId: oldestTodoId.current }
    });
```

### Types for fetching newer todos

Now let's apply the type definitions to `client.query` in loadNew function.

```javascript
  import {
    GetOldPublicTodosQuery,
    GetOldPublicTodosQueryVariables,
+   GetNewPublicTodosQuery,
+   GetNewPublicTodosQueryVariables,
    Todos
  } from '../../generated/graphql';

-   const { data, networkStatus } = await client.query({
+   const { data, networkStatus } = await client.query<GetNewPublicTodosQuery, GetNewPublicTodosQueryVariables>({
      query: GET_NEW_PUBLIC_TODOS,
      variables: {
        latestVisibleId: newestTodoId.current
      }
    });
```

### Types for useSubscription

In the useSubscription hook, let's apply the NotifyNewPublicTodosSubscription type definitions. We will also handle the `data` being undefined.

```javascript

  const TodoPublicListSubscription = () => {
    // Run a subscription to get the latest public todo
    const NOTIFY_NEW_PUBLIC_TODOS = gql`
      subscription notifyNewPublicTodos {
        todos(
          where: { is_public: { _eq: true } }
          limit: 1
          order_by: { created_at: desc }
        ) {
          id
          created_at
        }
      }
    `;

-   const { loading, error, data } = useSubscription(NOTIFY_NEW_PUBLIC_TODOS);
+   const { loading, error, data } = useSubscription<NotifyNewPublicTodosSubscription>(NOTIFY_NEW_PUBLIC_TODOS);
    ...
-   if (error) {
+   if (error || !data) {
      return <span>Error</span>;
    }
  };

```















