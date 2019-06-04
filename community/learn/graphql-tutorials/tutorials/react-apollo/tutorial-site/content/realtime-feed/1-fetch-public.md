---
title: "Fetch public todos - subscription"
metaTitle: "Fetch public todos using Subscription | GraphQL React Apollo Tutorial"
metaDescription: "You will learn how to make use of GraphQL Subscriptions to get notified whenever a new todo comes in React app"
---

import GithubLink from "../../src/GithubLink.js";
import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/Kero00_8bfk" />

Let's define the graphql query to be used:

Open `src/components/Todo/TodoPublicList.js` and add the following imports.

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/react-apollo/app-final/src/components/Todo/TodoPublicList.js" text="src/components/Todo/TodoPublicList.js" />

```javascript
import React, { Component, Fragment } from 'react';
+ import gql from 'graphql-tag';

import TaskItem from "./TaskItem";
```

Now let's define the subscription query to get notified about new public todos

```javascript
import React, { Component, Fragment } from 'react';
import gql from 'graphql-tag';

import TaskItem from "./TaskItem";

class TodoPublicList extends Component {
  ...
}

+ // Run a subscription to get the latest public todo
+ const NOTIFY_NEW_PUBLIC_TODOS = gql`
+  subscription notifyNewPublicTodos {
+    todos (where: { is_public: { _eq: true}}, limit: 1, order_by: {created_at: desc }) {
+      id
+      created_at
+    }
+  }
+ `;

export default TodoPublicList;
```

Also lets add a functional component which uses this subscription query.
Import `Subscription` from `react-apollo` to get started.

```javascript
import React, { Component, Fragment } from 'react';
+ import {Subscription} from 'react-apollo';
import gql from 'graphql-tag';

import TaskItem from "./TaskItem";

class TodoPublicList extends Component {
  ...
}

// Run a subscription to get the latest public todo
const NOTIFY_NEW_PUBLIC_TODOS = gql`
 subscription notifyNewPublicTodos {
   todos (where: { is_public: { _eq: true}}, limit: 1, order_by: {created_at: desc }) {
     id
     created_at
   }
 }
`;

+ const TodoPublicListSubscription = () => {
+  return (
+    <Subscription subscription={NOTIFY_NEW_PUBLIC_TODOS}>
+      {({loading, error, data}) => {
+        if (loading) {
+          return (<span>Loading...</span>);
+        }
+        if (error) {
+          return (<span>Error</span>);
+        }
+        return {};
+      }}
+    </Subscription>
+  );
+ };

export default TodoPublicList;
```

What does the Subscription do?
-----------------------------

The query fetches `todos` with a simple condition; `is_public` must be true. We also limit the number of todos to 1, since we would just like to get notified whenever a new todo comes in.
We sort the todos by its latest created_at time according to the schema. We specify which fields we need for the todos node.

Right now we don't return anything when new data comes in. We already have the TodoPublicList component which renders the list of public todos. So let's return that component.

```javascript
 const TodoPublicListSubscription = () => {
  return (
    <Subscription subscription={NOTIFY_NEW_PUBLIC_TODOS}>
      {({loading, error, data}) => {
        if (loading) {
          return (<span>Loading...</span>);
        }
        if (error) {
          return (<span>Error</span>);
        }
-       return {};
+       return (<TodoPublicList latestTodo={data.todos.length ? data.todos[0] : null} />);
      }}
    </Subscription>
  );
 };
```

We would like to now return the new TodoPublicListSubscription component which has the Subscription component integrated.

```javascript
- export default TodoPublicList;
+ export default TodoPublicListSubscription;
```
