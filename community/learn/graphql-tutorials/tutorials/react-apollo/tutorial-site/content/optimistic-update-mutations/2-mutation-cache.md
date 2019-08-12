---
title: "Mutation and update cache"
metaTitle: "Apollo client.mutate for GraphQL mutation update | GraphQL React Apollo Tutorial"
metaDescription: "We will use the Apollo client.mutate from withApollo HOC from react-apollo as an example to modify existing data and update cache locally using readQuery and writeQuery and handle optimisticResponse"
---

import GithubLink from "../../src/GithubLink.js";
import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/lXIQxuSZ588" />

Now let's do the integration part. Open `src/components/Todo/TodoItem.js` and add the following code below the other imports:

```javascript
+ import gql from 'graphql-tag';
```
Let's define the graphql mutation to update the completed status of the todo

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/react-apollo/app-final/src/components/Todo/TodoItem.js" text="src/components/Todo/TodoItem.js" />

```javascript
const TodoItem = ({index, todo}) => {

  const removeTodo = (e) => {
    e.preventDefault();
    e.stopPropagation();
  };

+  const TOGGLE_TODO = gql`
+    mutation toggleTodo ($id: Int!, $isCompleted: Boolean!) {
+      update_todos(where: {id: {_eq: $id}}, _set: {is_completed: $isCompleted}) {
+        affected_rows
+      }
+    }
+  `;

  const toggleTodo = () => {};

  return (
    ...
  );
};

export default TodoItem;

```

### Apollo client.mutate
We need to call `client.mutate` to make the mutation. To make sure we have access to `client`, we wrap our TodoItem component with `withApollo` like below:

```javascript
  import React from 'react';
+ import {withApollo} from 'react-apollo';
  import gql from 'graphql-tag';

  const TodoItem = ({index, todo}) => {
  };

- export default TodoItem;
+ export default withApollo(TodoItem);
```

`withApollo` will inject the `client` prop to the TodoItem component. Let's add it to the props list

```javascript
-  const TodoItem = ({index, todo}) => {
+  const TodoItem = ({index, todo, client}) => {
```

We already have the onChange handler toggleTodo for the input. Let's update the function to make a mutation.

```javascript
  const toggleTodo = () => {
+    client.mutate({
+      mutation: TOGGLE_TODO,
+      variables: {id: todo.id, isCompleted: !todo.is_completed},
+      optimisticResponse: {},
+    });
  };
```

The above code will just make a mutation, updating the todo's is_completed property in the database.
To update the cache, we will be using the `update` function again to modify the cache. We need to fetch the current list of todos from the cache before modifying it. So let's import the query.

```javascript
+ import {GET_MY_TODOS} from './TodoPrivateList';
```
Now let's add the code for `update` function.

```javascript
  const toggleTodo = () => {
    client.mutate({
      mutation: TOGGLE_TODO,
      variables: {id: todo.id, isCompleted: !todo.is_completed},
      optimisticResponse: {},
+      update: (cache) => {
+        const existingTodos = cache.readQuery({ query: GET_MY_TODOS });
+        const newTodos = existingTodos.todos.map(t => {
+          if (t.id === todo.id) {
+            return({...t, is_completed: !t.is_completed});
+          } else {
+            return t;
+          }
+        });
+        cache.writeQuery({
+          query: GET_MY_TODOS,
+          data: {todos: newTodos}
+        });
+      }
    });
  };

```

We are fetching the existing todos from the cache using `cache.readQuery` and updating the is_completed value for the todo that has been updated.

Finally we are writing the updated todo list to the cache using `cache.writeQuery`.
