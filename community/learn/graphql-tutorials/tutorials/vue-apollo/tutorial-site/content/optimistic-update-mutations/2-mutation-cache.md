---
title: "Mutation and update cache"
metaTitle: "Vue Apollo client.mutate for GraphQL mutation update | GraphQL Vue Apollo Tutorial"
metaDescription: "We will use the Apollo Client's $apollo.mutate from vue-apollo as an example to modify existing data and update cache locally using readQuery and writeQuery and handle optimisticResponse"
---

import GithubLink from "../../src/GithubLink.js";
import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/h4svDfN32s8" />

Now let's do the integration part. Open `src/components/TodoItem.vue` and add the following code below:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/vue-apollo/app-final/src/components/TodoItem.vue" text="src/components/TodoItem.vue" />

```javascript
<script>
+ import gql from 'graphql-tag';
  export default {
    ...
  }
</script>
```
Let's define the graphql mutation to update the completed status of the todo

```javascript
<script>
  import gql from 'graphql-tag';
+ const TOGGLE_TODO = gql`
+   mutation update_todos($id: Int!, $isCompleted: Boolean!) {
+    update_todos(where: { id: { _eq: $id } }, _set: { is_completed: $isCompleted }) {
+       affected_rows
+     }
+   }
+ `;
  export default {
    ...
  }
</script>

```

We need to call `this.$apollo.mutate` to make the mutation. 

```javascript
  export default {
    props: ['todos', 'type'],
    methods: {
      handleTodoToggle: function (todo) {
        // update todo data in db here
+       this.$apollo.mutate({
+         mutation: TOGGLE_TODO,
+         variables: {
+          id: todo.id,
+          isCompleted: !todo.is_completed
+         },
+         update: (store, { data: { update_todos } }) => {
+           if (update_todos.affected_rows) {
+             // eslint-disable-next-line
+             console.log(update_todos);
+           }
+         },
+       });
     },
```

The above code will just make a mutation, updating the todo's is_completed property in the database.
To update the cache, we will be using the `update` function again to modify the cache. We need to fetch the current list of todos from the cache before modifying it. So let's import the query.

```javascript
<script>
  import gql from 'graphql-tag';
+ import { GET_MY_TODOS } from "./TodoPrivateList.vue";
  const TOGGLE_TODO = gql`
    mutation update_todos($id: Int!, $isCompleted: Boolean!) {
      update_todos(where: { id: { _eq: $id } }, _set: { is_completed: $isCompleted }) {
        affected_rows
      }
    }
  `;
```
Now let's add the code for `update` function.

```javascript
  methods: {
    handleTodoToggle: function (todo) { // eslint-disable-line
      // update todo data in db here
      this.$apollo.mutate({
        mutation: TOGGLE_TODO,
        variables: {
         id: todo.id,
         isCompleted: !todo.is_completed
        },
        update: (store, { data: { update_todos } }) => {
          if (update_todos.affected_rows) {
-           // eslint-disable-next-line
-           console.log(update_todos);
+           if (this.type === "private") {
+             // Read the data from our cache for this query.
+             const data = store.readQuery({
+               query: GET_MY_TODOS,
+             });
+             const toggledTodo = data.todos.find(t => t.id === todo.id);
+             toggledTodo.is_completed = !todo.is_completed;
+             store.writeQuery({
+               query: GET_MY_TODOS,
+               data
+             });
+           } 
          }
        },
      });
    },

```

We are fetching the existing todos from the cache using `cache.readQuery` and updating the is_completed value for the todo that has been updated.

Finally we are writing the updated todo list to the cache using `cache.writeQuery`.

## Optimistic response

Now let's remove the input lag by adding `optimisticResponse`. This adds a fake result as soon as the network request is made so that the UI can render quickly.

```javascript
  handleTodoToggle: function (todo) { // eslint-disable-line
    // update todo data in db here
    this.$apollo.mutate({
      mutation: TOGGLE_TODO,
      variables: {
       id: todo.id,
       isCompleted: !todo.is_completed
      },
      update: (store, { data: { update_todos } }) => {
        if (update_todos.affected_rows) {
          if (this.type === "private") {
            // Read the data from our cache for this query.
            const data = store.readQuery({
              query: GET_MY_TODOS,
            });
            const toggledTodo = data.todos.find(t => t.id === todo.id);
            toggledTodo.is_completed = !todo.is_completed;
            store.writeQuery({
              query: GET_MY_TODOS,
              data
            });
          } 
        }
      },
+     optimisticResponse: {
+       __typename: 'Mutation',
+       update_todos: {
+         __typename: 'todos',
+         id: todo.id,
+         is_completed: !todo.is_completed,
+         affected_rows: 1
+       },
+     }
    });
  },
```

Now the fake result should appear quickly in case the network request takes more time. This should trigger a call to `update` function which will update the todo temporarily till the actual response from the server is received.

