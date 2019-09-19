---
title: "Fetch public todos - subscription"
metaTitle: "Fetch public todos using Subscription | GraphQL React Apollo Typescript Tutorial"
metaDescription: "You will learn how to make use of GraphQL Subscriptions to get notified whenever a new todo comes in React app"
---

import GithubLink from "../../src/GithubLink.js";

Open `src/components/Todo/TodoPublicList.tsx` and add the following imports.

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/typescript-react-apollo/app-final/src/components/Todo/TodoPublicList.tsx" text="src/components/Todo/TodoPublicList.tsx" />

```javascript

  import React, { Fragment, useState, useRef } from "react";
+ import { useSubscription } from "@apollo/react-hooks";
+ import gql from "graphql-tag";
  import TaskItem from "./TaskItem";

```

Now let's define the subscription query to get notified about new public todos. We will define a functional component which uses this subscription.

```javascript

  import React, { Fragment, useState, useRef } from "react";
  import { useSubscription } from "@apollo/react-hooks";
  import gql from "graphql-tag";
  import TaskItem from "./TaskItem";
  ...
  const TodoPublicList = (props: publicListProps) => {
    ...
  };

+ const TodoPublicListSubscription = () => {
+   // Run a subscription to get the latest public todo
+   const NOTIFY_NEW_PUBLIC_TODOS = gql`
+     subscription notifyNewPublicTodos {
+       todos(
+         where: { is_public: { _eq: true } }
+         limit: 1
+         order_by: { created_at: desc }
+       ) {
+         id
+         created_at
+       }
+     }
+   `;

+   const { loading, error, data } = useSubscription(NOTIFY_NEW_PUBLIC_TODOS);
+   if (loading) {
+     return <span>Loading...</span>;
+   }
+   if (error || !data) {
+     return <span>Error</span>;
+   }
+   return (
+     <TodoPublicList latestTodo={data.todos.length ? data.todos[0] : null} />
+   );
+ };

  export default TodoPublicList;

```

What does the Subscription do?
-----------------------------

The query fetches `todos` with a simple condition; `is_public` must be true. We also limit the number of todos to 1, since we would just like to get notified whenever a new todo comes in.
We sort the todos by its latest created_at time according to the schema. We specify which fields we need for the todos node.

We already have the TodoPublicList component which renders the list of public todos. So let's return that component.

We would like to now return the new TodoPublicListSubscription component which has the Subscription component integrated.

```javascript
- export default TodoPublicList;
+ export default TodoPublicListSubscription;
```

Alright, now there's a subscription running listening to any new todos being added to the public list.
