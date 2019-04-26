---
title: "<Query> component"
---

In this section, we will implement GraphQL Queries and integrate with the Vue UI.
With Apollo Client, you can send queries in 3 different ways.

1. Using the `apollo` object (Recommended).
2. Using `$apollo`.
3. Using Apollo Components.

The recommended method is to use the apollo object, where you will just pass your GraphQL query in the apollo component options and it will fetch the data automatically and will present it in the component data.

Great! Now let's define the graphql query to be used:

Open `src/components/TodoPrivateList.vue` and add the following code:

```javascript
  <script>
  import TodoItem from "../components/TodoItem";
  import TodoFilters from "../components/TodoFilters";
+ import gql from 'graphql-tag';

+ const GET_MY_TODOS = gql`
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
+ apollo: {
+   todos: {
+     // graphql query
+     query: GET_MY_TODOS,
+   },
+ },
}

```

Remember that we included `ApolloProvider` in our Vue app. We are using the same client prop to send it down to the components.

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
     ]
   }
 },

```

Finally, update the exports to render the function returning the `<Query>` component.

```javascript
- export default TodoPrivateList;
+ export default TodoPrivateListQuery;
+ export {GET_MY_TODOS};
```

Then, we wrap the new functional component with `Query` passing our graphql query.

Woot! You have written your first GraphQL integration with React. Easy isn't it?

How does this work?
-------------------
When you wrapped your return with `<Query>` component, Apollo injected props into the component’s render prop function. Most important ones are:

`loading`: A boolean that indicates whether the request is in flight. If loading is true, then the request hasn't finished. Typically this information can be used to display a loading spinner.

`error`: A runtime error with graphQLErrors and networkError properties. Contains information about what went wrong with your query.

`data`: An object containing the result of your GraphQL query. This will contain our actual data from the server. In our case, it will be the todo data.

You can read more about other render props that Apollo passes [here](https://www.apollographql.com/docs/react/essentials/queries.html#render-prop)

Using the `data` prop, we are parsing the results from the server. In our query, `data` prop has an array `todos` which can be mapped over to render each `TodoItem`.

If you noted, there has been some client side filtering to the todos that are displayed.
