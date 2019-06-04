---
title: "Sync new todos"
metaTitle: "Sync new todos in public feed | GraphQL React Apollo Tutorial"
metaDescription: "You will learn how to sync new todos added by other people in the public feed by fetching older and newer data using GraphQL Queries"
---

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/0_tCwIuoBaM" />

Once a new todo is entered in a public list, it needs to appear in the UI. Instead of automatically displaying the todo in the UI, we use a Feed like Notification banner which appears whenever a new todo is received.

Remember that previously we updated the cache using the cache API and the UI got updated automatically, because updating the cache triggered a re-render for those components that were subscribed to this store.

We are not going to use that approach here since we don't want public list UI to be automatically updated.

In the Subscription component of the previous step, we only get the latest todo and not the existing list. We will now write a simple query to fetch the list of existing public todos.

Start off by wrapping the existing component with `withApollo` HOC to get access to the `client` prop to make queries.

```javascript
import React, { Component, Fragment } from 'react';
- import {Subscription} from 'react-apollo';
+ import {Subscription, withApollo} from 'react-apollo';
import gql from 'graphql-tag';

import TaskItem from "./TaskItem";

- class TodoPublicList extends Component {
+ class _TodoPublicList extends Component {
   ...
  }

+ const TodoPublicList = withApollo(_TodoPublicList);
```

Now that we have access to the client prop, let's update the `_TodoPublicList` component

```javascript
class _TodoPublicList extends Component {
-  constructor() {
+  constructor(props) {
-   super();
+   super(props);

    this.state = {
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
    };

    this.loadNew = this.loadNew.bind(this);
    this.loadOlder = this.loadOlder.bind(this);

+   this.client = props.client;
+   this.oldestTodoId = props.latestTodo.id + 1;
+   this.newestTodoId = props.latestTodo.id;

  }

  loadNew() {}

  loadOlder() {}

  render() {
    ...
  }
}
```

Let's populate initial state by fetching the existing list of todos in `componentDidMount()`

```javascript
class _TodoPublicList extends Component {
  constructor(props) {
    ...
  }

  loadNew() {
  }

  loadOlder() {
  }

+  componentDidMount() {
+    this.loadOlder();
+  }

   render() {
     ...
   }
}
```

Update the `loadOlder` method to the following:

```javascript
  loadOlder() {
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
+    this.client.query({
+      query: GET_OLD_PUBLIC_TODOS,
+      variables: {oldestTodoId: (this.oldestTodoId)}
+    })
+    .then(({data}) => {
+      if (data.todos.length) {
+        this.oldestTodoId = data.todos[data.todos.length - 1].id;
+        this.setState({todos: [...this.state.todos, ...data.todos]});
+      } else {
+        this.setState({olderTodosAvailable: false});
+      }
+    })
+    .catch(error => {
+      console.error(error);
+      this.setState({error: true});
+    });
  }
```

We are defining a query to fetch older public todos and making a `client.query` call to get the data from the database. Once we get the data, we update the `todos` state to re-render the UI with the available list of public todos.

Try adding a new todo in the public feed and notice that it will not show up on the UI. Now refresh the page to see the added todo.

This happens because we haven't yet implemented a way to show the newly added todo to the feed.

Let's handle that in `componentDidUpdate()` lifecycle method

```javascript
+ componentDidUpdate(prevProps) {
+  // Do we have a new todo available?
+  if (this.props.latestTodo.id > this.newestTodoId) {
+    this.newestTodoId = this.props.latestTodo.id;
+    this.setState({newTodosCount: this.state.newTodosCount + 1});
+  }
+ }

  componentDidMount() {
    ...
  }
```

Now try adding a new todo to the public feed and you will see the notification appearing saying that a new task has arrived.

Great! We still have one functionality left. When a new task arrives on the public feed and when the user clicks on the New tasks section, we should make a query to re-fetch the todos that are not present on our current public feed.

Update `loadNew()` method with the following code

```javascript
  loadNew() {
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
+   this.client.query({
+     query: GET_NEW_PUBLIC_TODOS,
+     variables: {latestVisibleId: this.state.todos[0].id}
+   })
+   .then(({data}) => {
+     this.newestTodoId = data.todos[0].id;
+     this.setState({todos: [...data.todos, ...this.state.todos], newTodosCount: 0});
+   })
+   .catch(error => {
+     console.error(error);
+     this.setState({error: true});
+   });
  }
```