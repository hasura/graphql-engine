---
title: "Smart Query"
metaTitle: "Vue Apollo Smart Query | GraphQL Vue Apollo Tutorial"
metaDescription: "We will use the Smart Query from vue-apollo. Each query in apollo object becomes a smart query and it will be executed automatically when the component is mounted."
---

import GithubLink from "../../src/GithubLink.js";
import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/kH2P4VPux24" />

In this section, we will implement GraphQL Queries and integrate with the Vue UI.
With Apollo Client, you can send queries in 3 different ways.

1. Using the `apollo` object (Recommended)
2. Using `$apollo`
3. Using Apollo Components

The recommended method is to use the apollo object, where you will just pass your GraphQL query in the apollo component options and it will fetch the data automatically and will present it in the component data. Each one of them will become a `smart query`. A smart query will be executed automatically when the component is mounted and the response data will be available for the component to consume.

Great! Now let's define the graphql query to be used:

Open `src/components/TodoPrivateList.vue` and add the following code:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/vue-apollo/app-final/src/components/TodoPrivateList.vue" text="src/components/TodoPrivateList.vue" />

```javascript
  <script>
  import TodoItem from "../components/TodoItem";
  import TodoFilters from "../components/TodoFilters";
+ import gql from 'graphql-tag';

+ export const GET_MY_TODOS = gql`
+  query getMyTodos {
+    todos(where: { is_public: { _eq: false} }, order_by: { created_at: desc }) {
+      id
+      title
+      created_at
+      is_completed
+  }
+ }`;

```

We have now written the graphql query as a javascript constant using the `gql` parser function. This function is used to parse the plain string as a graphql query.

What does this query do? 
------------------------
The query fetches `todos` with a simple condition; `is_public` must be false. We sort the todos descending by its `created_at` time according to the schema. We specify which fields we need for the todos node.

The query is now ready, let's integrate it with our Vue component.

```javascript
<script>
  export default {
    components: {
      TodoItem, TodoFilters
    },
    data() {
      ...
    },
    computed: {
      ...
    },
    methods: {
      ...
    },
+   apollo: {
+     todos: {
+       // graphql query
+       query: GET_MY_TODOS,
+     },
+   },
  }

```

Remember that we included `ApolloProvider` in our Vue app. This allows us to use the apollo object definition.

Let's remove the mock `todos` data which was used to populate sample data.

```javascript
export default {
  components: {
    TodoItem, TodoFilters
  },
  data() {
    return {
      type: "private",
      filterType: "all",
      todos: [
-       {
-         id: "1",
-         title: "This is private todo 1",
-         is_completed: true,
-         is_public: false
-       },
-       {
-         id: "2",
-         title: "This is private todo 2",
-         is_completed: false,
-         is_public: false
-       }
     ],
   }
 },

```

Woot! You have written your first GraphQL integration with Vue. Easy isn't it?

How does this work?
-------------------
Each query declared in the apollo definition (that is, which doesn't start with a $ char) in a component results in the creation of a smart query object.

## Properties
You have access to the following properties:

`loading`: A boolean that indicates whether the request is in flight. If loading is true, then the request hasn't finished. Typically this information can be used to display a loading spinner.

You can read more about other properties that Apollo passes [here](https://github.com/Akryum/vue-apollo/blob/master/docs/api/smart-query.md)

## Hooks
You can write a hook in your apollo definition to handle errors.

`error`: A runtime error with graphQLErrors and networkError properties. Contains information about what went wrong with your query.

Remember that we had defined the apollo object with `todos` smart query. The server returns an array todos which can be mapped over to render each `TodoItem`.

If you noted, there has been some client side filtering to the todos that are displayed. You can see this in the methods, `filterResults` to filter todos based on whether they were active or completed.
