---
title: "Sync new todos"
metaTitle: "Sync new todos in public feed | GraphQL React Apollo Typescript Tutorial"
metaDescription: "You will learn how to sync new todos added by other people in the public feed by fetching older and newer data using GraphQL Queries"
---

Once a new todo is entered in a public list, it needs to appear in the UI. Instead of automatically displaying the todo in the UI, we use a Feed like Notification banner which appears whenever a new todo is received.

Remember that previously we updated the cache using the cache API and the UI got updated automatically, because updating the cache triggered a re-render for those components that were subscribed to this store.

We are not going to use that approach here since we don't want public list UI to be automatically updated.

In the Subscription component of the previous step, we only get the latest todo and not the existing list. We will now write a simple query to fetch the list of existing public todos.

Start off by importing the `useApolloClient` hook to get access to the `client` prop to make queries manually.

```javascript

  import React, { Fragment, useState, useEffect, useRef } from "react";
- import { useSubscription } from "@apollo/react-hooks";
+ import { useSubscription, useApolloClient } from "@apollo/react-hooks";
  ...
  const TodoPublicList = (props: publicListProps) => {
    const [olderTodosAvailable] = useState(props.latestTodo ? true : false)
    const [newTodosCount] = useState(0)
    const initialTodos = [
      ...
    ];
    ...
    if(todos && todos.length) {
      oldestTodoId.current = todos[todos.length - 1].id
      newestTodoId.current = todos[0].id;
    }
+   const client = useApolloClient();

```

Let's populate initial state by fetching the existing list of todos in `useEffect` hook.

```javascript

- import React, { Fragment, useState, useRef } from "react";
+ import React, { Fragment, useState, useRef, useEffect } from "react";
  const TodoPublicList = (props: publicListProps) => {
    ...
    const loadOlder = () => {
    };

    const loadNew = () => {
    };
+   useEffect(() => {
+     loadOlder();
+     // eslint-disable-next-line
+   }, []);

```

Update the `loadOlder` method to the following:

```javascript

- const loadOlder = () => {
+ const loadOlder = async () => {
+   const GET_OLD_PUBLIC_TODOS = gql`
+     query getOldPublicTodos($oldestTodoId: Int!) {
+       todos(
+         where: { is_public: { _eq: true }, id: { _lt: $oldestTodoId } }
+         limit: 7
+         order_by: { created_at: desc }
+       ) {
+         id
+         title
+         created_at
+         user {
+           name
+         }
+       }
+     }
+   `;

+   const { data, networkStatus } = await client.query({
+     query: GET_OLD_PUBLIC_TODOS,
+     variables: { oldestTodoId: oldestTodoId.current }
+   });
+ };
```

We are defining a query to fetch older public todos and making a `client.query` call to get the data from the database. Once we get the data, we need to update the `todos` state to re-render the UI with the available list of public todos.

```javascript

- const [olderTodosAvailable] = useState(props.latestTodo ? true : false)
+ const [olderTodosAvailable, setOlderTodosAvailable] = useState(props.latestTodo ? true : false)
- const [newTodosCount] = useState(0)
+ const [newTodosCount, setNewTodosCount] = useState(0)
  const initialTodos = [
    ...
  ];
+ const [error, setError] = useState(false);
- const [todos] = useState<TodoItem[]>(initialTodos);
+ const [todos, setTodos] = useState<TodoItem[]>(initialTodos);
+   if (data.todos && data.todos.length) {
+     setTodos(prevTodos => {
+       if(prevTodos) {
+         return [...prevTodos, ...data.todos];
+       } else {
+         return data.todos;
+       }
+     });
+     oldestTodoId.current = data.todos[data.todos.length - 1].id;
+   } else {
+     setOlderTodosAvailable(false);
+   }
+   if (networkStatus === 8) {
+     console.error(data);
+     setError(true);
+   }

```

Try adding a new todo in the public feed and notice that it will not show up on the UI. Now refresh the page to see the added todo.

This happens because we haven't yet implemented a way to show the newly added todo to the feed.

Let's handle that in `useEffect` hook. 

```javascript
  useEffect(() => {
    loadOlder();
    // eslint-disable-next-line
  }, []);

+ useEffect(
+   () => {
+     if (props.latestTodo && props.latestTodo.id! > newestTodoId.current) {
+       setNewTodosCount(n => n + 1);
+       newestTodoId.current = props.latestTodo.id!;
+     }
+   },
+   [props.latestTodo]
+ );
+
+ if(error) {
+   return (<div>Error...</div>);
+ }
```

We are also handling the error case above.

Finally, let's remove the dummy initial state for todos and set inital state for todos to be an empty array `[]`.

```javascript

  const TodoPublicList = (props: publicListProps) => {
    const [olderTodosAvailable, setOlderTodosAvailable] = useState(props.latestTodo ? true : false)
    const [newTodosCount, setNewTodosCount] = useState(0)
-   const initialTodos = [
-     {
-       id: 1,
-       title: "This is public todo 1",
-       user: {
-         name: "someUser1"
-       }
-     },
-     {
-       id: 2,
-       title: "This is public todo 2",
-       is_completed: false,
-       is_public: true,
-       user: {
-         name: "someUser2"
-       }
-     },
-     {
-       id: 3,
-       title: "This is public todo 3",
-       user: {
-         name: "someUser3"
-       }
-     },
-     {
-       id: 4,
-       title: "This is public todo 4",
-       user: {
-         name: "someUser4"
-       }
-     }
-   ];
    const [error, setError] = useState(false);
-   const [todos, setTodos] = useState<TodoItem[]>(initialTodos);
+   const [todos, setTodos] = useState<TodoItem[]>([]);
```

Now try adding a new todo to the public feed and you will see the notification appearing saying that a new task has arrived.

Great! We still have one functionality left. When a new task arrives on the public feed and when the user clicks on the New tasks section, we should make a query to re-fetch the todos that are not present on our current public feed.

Update `loadNew()` method with the following code

```javascript

- const loadNew = () => {
+ const loadNew = async () => {
+   const GET_NEW_PUBLIC_TODOS = gql`
+     query getNewPublicTodos($latestVisibleId: Int) {
+       todos(
+         where: { is_public: { _eq: true }, id: { _gt: $latestVisibleId } }
+         order_by: { created_at: desc }
+       ) {
+         id
+         title
+         created_at
+         user {
+           name
+         }
+       }
+     }
+   `;
+
+   const { data, networkStatus } = await client.query({
+     query: GET_NEW_PUBLIC_TODOS,
+     variables: {
+       latestVisibleId: newestTodoId.current
+     }
+   });
+
+    if (data && data.todos) {
+     setTodos(prevState => {
+       if(prevState) {
+         return [...data.todos, ...prevState]
+       } else {
+         return data.todos;
+       }
+     });
+     setNewTodosCount(0);
+     newestTodoId.current = data.todos[0].id;
+   }
+   if (networkStatus === 8) {
+     console.error(data);
+     setError(true);
+   }
  };

```

Similar to loading older todos, we are making a query to fetch newer todos but with the condition of todo id > latest visible todo's id.
