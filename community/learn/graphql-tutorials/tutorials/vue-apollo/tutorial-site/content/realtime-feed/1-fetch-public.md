---
title: "Fetch public todos - subscription"
metaTitle: "Fetch public todos using Subscription | GraphQL Vue Apollo Tutorial"
metaDescription: "You will learn how to make use of GraphQL Subscriptions to get notified whenever a new todo comes in Vue app"
---

import GithubLink from "../../src/GithubLink.js";
import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/ooy-5LzrvxM" />

Let's define the graphql query to be used:

Open `src/components/TodoPublicList.vue` and add the following imports.

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/vue-apollo/app-final/src/components/TodoPublicList.vue" text="src/components/TodoPublicList.vue" />

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

