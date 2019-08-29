---
title: "Sync new todos"
metaTitle: "Sync new todos in public feed | GraphQL React Apollo Hooks Tutorial"
metaDescription: "You will learn how to sync new todos added by other people in the public feed by fetching older and newer data using GraphQL Queries"
---

Once a new todo is entered in a public list, it needs to appear in the UI. Instead of automatically displaying the todo in the UI, we use a Feed like Notification banner which appears whenever a new todo is received.

Remember that previously we updated the cache using the cache API and the UI got updated automatically, because updating the cache triggered a re-render for those components that were subscribed to this store.

We are not going to use that approach here since we don't want public list UI to be automatically updated.

In the `TodoPublicListSubscription` component of the previous step, we only get the latest todo and not the existing list. We will now write a simple query to fetch the list of existing public todos.

Start off by importing `useEffect` from  `react` and `useApolloClient` from `@apollo/react-hooks`

```javascript
- import React, { Fragment, useState } from "react";
+ import React, { Fragment, useState, useEffect } from "react";
- import { useSubscription } from "@apollo/react-hooks";
+ import { useSubscription, useApolloClient } from "@apollo/react-hooks";
import gql from 'graphql-tag';

import TaskItem from "./TaskItem";
```

Now that we have access to the client, let's update the `TodoPublicList` component

```javascript
const TodoPublicList = props => {
    const [state, setState] = useState({
-     olderTodosAvailable: true,
+     olderTodosAvailable: props.latestTodo ? true : false,
-     newTodosCount: 1,
+     newTodosCount: 0,
      todos: [
-       {
-         id: "1",
-         title: "This is public todo 1",
-         user: {
-           name: "someUser1"
-         }
-       },
-       {
-         id: "2",
-         title: "This is public todo 2",
-         is_completed: false,
-         is_public: true,
-         user: {
-           name: "someUser2"
-         }
-       },
-       {
-         id: "3",
-         title: "This is public todo 3",
-         user: {
-           name: "someUser3"
-         }
-       },
-       {
-         id: "4",
-         title: "This is public todo 4",
-         user: {
-           name: "someUser4"
-         }
-       }
      ],
+     error: false
    });

+  let numTodos = state.todos.length;
+  let oldestTodoId = numTodos
+    ? state.todos[numTodos - 1].id
+    : props.latestTodo
+      ? props.latestTodo.id + 1
+      : 0;
+  let newestTodoId = numTodos
+    ? state.todos[0].id
+    : props.latestTodo
+      ? props.latestTodo.id
+      : 0;
+
+  const client = useApolloClient();

  }

  const loadNew = () => {};

  const loadOlder = () => {};

  ...
}
```

Let's populate initial state by fetching the existing list of todos in `useEffect`

```javascript
const TodoPublicList = props => {
  ...

  const client = useApolloClient();

+  useEffect(() => {
+    loadOlder();
+  }, []);

  const loadNew = () => {};

  const loadOlder = () => {}

  ...
}
```

Update the `loadOlder` method to the following:

```javascript
  const loadOlder = async () => {
+    const GET_OLD_PUBLIC_TODOS = gql`
+      query getOldPublicTodos ($oldestTodoId: Int!) {
+        todos (where: { is_public: { _eq: true}, id: {_lt: $oldestTodoId}}, limit: 7, order_by: { created_at: desc }) {
+          id
+          title
+          created_at
+          user {
+            name
+          }
+        }
+      }`;
+
+   const { error, data } = await client.query({
+      query: GET_OLD_PUBLIC_TODOS,
+      variables: { oldestTodoId: oldestTodoId }
+    });
+
+    if (data.todos.length) {
+      setState(prevState => {
+        return { ...prevState, todos: [...prevState.todos, ...data.todos] };
+      });
+      oldestTodoId = data.todos[data.todos.length - 1].id;
+    } else {
+      setState(prevState => {
+        return { ...prevState, olderTodosAvailable: false };
+      });
+    }
+    if (error) {
+      console.error(error);
+      setState(prevState => {
+        return { ...prevState, error: true };
+      });
+    }
+ }
```

We are defining a query to fetch older public todos and making a `client.query` call to get the data from the database. Once we get the data, we update the `todos` state to re-render the UI with the available list of public todos.

Try adding a new todo in the public feed and notice that it will not show up on the UI. Now refresh the page to see the added todo.

This happens because we haven't yet implemented a way to show the newly added todo to the feed.

Let's handle that in `useEffect` for on update

```javascript
  useEffect(() => {
    loadOlder();
  }, []);

+  useEffect(
+    () => {
+      if (props.latestTodo && props.latestTodo.id > newestTodoId) {
+        setState(prevState => {
+          return { ...prevState, newTodosCount: prevState.newTodosCount + 1 };
+        });
+        newestTodoId = props.latestTodo.id;
+      }
+    },
+    [props.latestTodo]
+  );
```

Now try adding a new todo to the public feed and you will see the notification appearing saying that a new task has arrived.

Great! We still have one functionality left. When a new task arrives on the public feed and when the user clicks on the New tasks section, we should make a query to re-fetch the todos that are not present on our current public feed.

Update `loadNew()` method with the following code

```javascript
  const loadNew = async () => {
+   const GET_NEW_PUBLIC_TODOS = gql`
+     query getNewPublicTodos ($latestVisibleId: Int!) {
+       todos(where: { is_public: { _eq: true}, id: {_gt: $latestVisibleId}}, order_by: { created_at: desc }) {
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
+   const { error, data } = await client.query({
+      query: GET_NEW_PUBLIC_TODOS,
+      variables: {
+        latestVisibleId: state.todos.length ? state.todos[0].id : null
+      }
+    });
 
+    if (data) {
+      setState(prevState => {
+        return {
+          ...prevState,
+          todos: [...data.todos, ...prevState.todos],
+          newTodosCount: 0
+        };
+      });
+      newestTodoId = data.todos[0].id;
+    }
+    if (error) {
+      console.error(error);
+      setState(prevState => {
+        return { ...prevState, error: true };
+      });
+    } 
  }
```