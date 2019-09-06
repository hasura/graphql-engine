---
title: "Sync new todos"
metaTitle: "Sync new todos in public feed | GraphQL Vue Apollo Tutorial"
metaDescription: "You will learn how to sync new todos added by other people in the public feed by fetching older and newer data using GraphQL Queries"
---

import GithubLink from "../../src/GithubLink.js";
import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/S9zkRckfWCY" />

Once a new todo is entered in a public list, it needs to appear in the UI. Instead of automatically displaying the todo in the UI, we use a Feed like Notification banner which appears whenever a new todo is received.

Remember that previously we updated the cache using the cache API and the UI got updated automatically, because updating the cache triggered a re-render for those components that were subscribed to this store.

We are not going to use that approach here since we don't want public list UI to be automatically updated.

In the Subscription query of the previous step, we only get the latest todo and not the existing list. We will now write a simple query to fetch the list of existing public todos.

Let's define the graphql query to fetch the list of existing public todos.

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/vue-apollo/app-final/src/components/TodoPublicList.vue" text="src/components/TodoPublicList.vue" />

```javascript
  <script>
  import gql from "graphql-tag";
  import TodoItem from "../components/TodoItem";
  import TodoFilters from "../components/TodoFilters";
  const NOTIFY_NEW_PUBLIC_TODOS = gql`
    subscription notifyNewPublicTodos {
      todos(
        where: { is_public: { _eq: true }} 
        order_by: { created_at: desc }
        limit: 1
      ) {
        id
        title
        created_at
      }
    }
  `;
+ const GET_OLD_PUBLIC_TODOS = gql`
+   query getOldPublicTodos($oldestTodoId: Int) {
+     todos(
+       where: { is_public: { _eq: true }, id: { _lt: $oldestTodoId } }
+       limit: 7
+       order_by: { created_at: desc }
+     ) {
+       id
+       title
+       created_at
+       is_public
+       user {
+         name
+       }
+     }
+   }
+ `;
```

In the `mounted()` lifecycle method, write the following code:

```javascript
  mounted() {
+   const that = this;
+   this.$apollo
+     .query({
+       query: GET_OLD_PUBLIC_TODOS
+     })
+     .then(data => {
+       this.todos = data.data.todos;
+       // start a subscription
+       this.$apollo
+         .subscribe({
+           query: NOTIFY_NEW_PUBLIC_TODOS,
+         })
+         .subscribe({
+           next(data) {
+             if (data.data.todos.length) {
+               // check if the received todo is already present
+               if(data.data.todos[0].id !== that.todos[0].id) {
+                 that.newTodosCount = that.newTodosCount + data.data.todos.length;
+               }
+             }
+           },
+           error(err) {
+             console.error("err", err);
+           }
+         });
+     });
  },
```

First, we are making a query to fetch the existing list. Then we are making a subscription to listen to any new todos after the latest todo `id` that is present in the vue app.

Since we have actual data coming from the server, let's remove the mock data.

```javascript
  data: function() {
    return {
      olderTodosAvailable: true,
-     newTodosCount: 1,
+     newTodosCount: 0,
      limit: 7,
      todos: [
-       {
-         id: "1",
-         title: "This is public todo 1",
-         is_public: true,
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
-         is_public: true,
-         user: {
-           name: "someUser3"
-         }
-       },
-       {
-         id: "4",
-         title: "This is public todo 4",
-         is_public: true,
-         user: {
-           name: "someUser4"
-         }
-       }
      ],
      type: "public"
    }
  },
```

Update the `loadOlder` method to the following:

```javascript
  methods: {
    loadMoreClicked: function() {
    },
    loadOlderClicked: function() {
+     this.$apollo
+       .query({
+         query: GET_OLD_PUBLIC_TODOS,
+         variables: {
+           oldestTodoId: this.todos.length
+             ? this.todos[this.todos.length - 1].id
+             : null
+         }
+       })
+       .then(data => {
+         if (data.data.todos.length) {
+           const mergedTodos = this.todos.concat(data.data.todos);
+           // update state with new todos
+           this.todos = mergedTodos;
+         } else {
+           this.olderTodosAvailable = false;
+         }
+       });
    },
  }
```

We are defining a query to fetch older public todos and making a `this.$apollo.query` call to get the data from the database. Once we get the data, we update the `todos` state to re-render the UI with the available list of public todos.

Try adding a new todo in the public feed and notice that it will not show up on the UI. Now refresh the page to see the added todo.

This happens because we haven't yet implemented a way to show the newly added todo to the feed.

But you will see the notification appearing saying that a new task has arrived.

Great! We still have one functionality left. When a new task arrives on the public feed and when the user clicks on the New tasks section, we should make a query to re-fetch the todos that are not present on our current public feed.

Let's define the query to fetch newer todos.

```javascript
  <script>
  import gql from "graphql-tag";
  import TodoItem from "../components/TodoItem";
  import TodoFilters from "../components/TodoFilters";
  const NOTIFY_NEW_PUBLIC_TODOS = gql`
    ...
  `;
  const GET_OLD_PUBLIC_TODOS = gql`
    ...
  `;
+ const GET_NEW_PUBLIC_TODOS = gql`
+   query getNewPublicTodos($latestVisibleId: Int!) {
+     todos(
+       where: { is_public: { _eq: true }, id: { _gt: $latestVisibleId } }
+       order_by: { created_at: desc }
+     ) {
+       id
+       title
+       created_at
+       is_public
+       user {
+         name
+       }
+     }
+   }
+ `;
```

Update `loadNew()` method with the following code

```javascript
  methods: {
    loadMoreClicked: function() {
+     this.newTodosCount = 0;
+     this.$apollo
+       .query({
+         query: GET_NEW_PUBLIC_TODOS,
+         variables: {
+           latestVisibleId: this.todos.length ? this.todos[0].id : null
+         }
+       })
+       .then(data => {
+         if (data.data.todos.length) {
+           const mergedTodos = data.data.todos.concat(this.todos);
+           // update state with new todos
+           this.todos = mergedTodos;
+         }
+       });
    },
```