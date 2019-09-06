---
title: "Run mutation, update cache"
metaTitle: "Vue Apollo Mutation using $apollo.mutate | GraphQL Vue Apollo Tutorial"
metaDescription: "We will use the Apollo client mutation method $apollo.mutate from vue-apollo as an example to insert new data and update cache locally using readQuery and writeQuery."
---

import GithubLink from "../../src/GithubLink.js";
import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/qHPGWfpqQ3o" />

Now let's do the integration part. Open `src/components/TodoInput.vue` and add the following code below to make the mutation.

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/vue-apollo/app-final/src/components/TodoInput.vue" text="src/components/TodoInput.vue" />

```javascript
export default {
  props: ['type'],
  data() {
    return {
      newTodo: '',
    }
  },
  methods: {
    addTodo: function () {
      // insert new todo into db
+     const title = this.newTodo && this.newTodo.trim()
+     const isPublic = this.type === "public" ? true : false;
+     this.$apollo.mutate({
+       mutation: ADD_TODO,
+       variables: {
+         todo: title,
+         isPublic: isPublic
+       },
+       update: (cache, { data: { insert_todos } }) => {
+         // Read the data from our cache for this query.
+         // eslint-disable-next-line
+         console.log(insert_todos);
+       },
+     });
   },
  }
}
```

## this.$apollo.mutate()

In the `addTodo` function defined above, we first define the values for title of the todo and the type of the todo (private or public?). Then in order to do a mutation, we make use of `this.$apollo.mutate()`.

The mutate function optionally takes variables, optimisticResponse and update; You are going to make use of the `update` function later. Right now we have written a console.log to test if the mutation works as expected.

Note that we are passing the variables todo and isPublic as required for the mutation to work.

The mutation has been integrated and the new todos will be inserted into the database. But the UI doesn't know that a new todo has been added. We need a way to tell Apollo Client to update the query for the list of todos.

## update cache

The `update` function comes in handy to update the cache for this mutation. It comes with utility functions such as `readQuery` and `writeQuery` that helps in reading from and writing to the cache.

Let's implement `update` for the above mutation.

We need to fetch the current list of todos from the cache. So let's import the `GET_MY_TODOS` query that we defined in the `TodoPrivateList.vue` component.

In the same file `TodoInput.vue`, make the following updates:

```javascript
<script>
  import gql from "graphql-tag";
+ import { GET_MY_TODOS } from "./TodoPrivateList.vue";
  ...
</script>
```

Let's modify the update function to read and write to cache.

```javascript
methods: {
  addTodo: function () {
    // insert new todo into db
    const title = this.newTodo && this.newTodo.trim()
    const isPublic = this.type === "public" ? true : false;
    this.$apollo.mutate({
      mutation: ADD_TODO,
      variables: {
        todo: title,
        isPublic: isPublic
      },
      update: (cache, { data: { insert_todos } }) => {
        // Read the data from our cache for this query.
-       // eslint-disable-next-line
-       console.log(insert_todos);
+       try {
+         if (this.type === "private") {
+           const data = cache.readQuery({
+             query: GET_MY_TODOS
+           });
+           const insertedTodo = insert_todos.returning;
+           data.todos.splice(0, 0, insertedTodo[0]);
+           cache.writeQuery({
+             query: GET_MY_TODOS,
+             data
+           });
+         }
+       } catch (e) {
+         console.error(e);
+       }
      },
    });
  },
}
```

Let's dissect what's happening in this code snippet.

Our goals were simple:

- Make a mutation to insert the new todo in the database.
- Once the mutation is done, we need to update the cache to update the UI.

The update function is used to update the cache after a mutation occurs.
It receives the result of the mutation (data) and the current cache (store) as arguments. You will then use these arguments to manage your cache so that the UI will be up to date.

cache.readQuery
---------------

Unlike `client.query`, readQuery will never make a request to your GraphQL server. It will always read from the cache. So we make a read request to the cache to get the current list of todos.

cache.writeQuery
----------------

We have already done the mutation to the graphql server using the mutate function. Our goal was to update the UI. This is where writeQuery comes to the rescue. writeQuery will allow you to change data in your local cache, but it is important to remember that they will not change any data on your server (exactly what we need).

  Any subscriber to the Apollo Client store will instantly see this update and render new UI accordingly.

We concatenate our new todo from our mutation with the list of existing todos and write the query back to the cache with cache.writeQuery

Now, the TodoPrivateList component using the apollo object with the same query will get the updated todo list as it is automatically subscribed to the store.

Great! That was actually easy :)

Let's wrap this by adding a line of code to clear the input value once the mutation is successful.

```javascript
export default {
  props: ['type'],
  data() {
    return {
      newTodo: '',
    }
  },
  methods: {
    addTodo: function () {
      ...
+     this.newTodo = '';
    },
  }
}
```

