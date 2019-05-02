---
title: "Fetch public todos - subscription"
---

Let's define the graphql query to be used:

Open `src/components/TodoPublicList.vue` and add the following imports.

```javascript
  <script>
+ import gql from "graphql-tag";
  import TodoItem from "../components/TodoItem";
  import TodoFilters from "../components/TodoFilters";
```

Now let's define the subscription query to get notified about new public todos

```javascript
  <script>
  import gql from "graphql-tag";
  import TodoItem from "../components/TodoItem";
  import TodoFilters from "../components/TodoFilters";
+ const NOTIFY_NEW_PUBLIC_TODOS = gql`
+   subscription notifyNewPublicTodos {
+     todos(
+       where: { is_public: { _eq: true }} 
+       order_by: { created_at: desc }
+       limit: 1
+     ) {
+       id
+       title
+       created_at
+     }
+   }
+ `;
```

What does the Subscription do?
------------------------------

The query fetches `todos` with a simple condition; `is_public` must be true and we limit the results to 1. We would just like to get notified whenever a new todo comes in.
We sort the todos by its latest created_at time according to the schema. We specify which fields we need for the todos node.

