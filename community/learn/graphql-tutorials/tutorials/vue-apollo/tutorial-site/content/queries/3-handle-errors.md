---
title: "Handle loading/errors"
metaTitle: "Vue Apollo Error Handling | GraphQL Vue Apollo Tutorial"
metaDescription: "We will handle the GraphQL loading and error states using the Vue Apollo properties and hooks - loading and error "
---

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/zF0SesqsGFg" />

As we saw in the previous step, Apollo gives access to properties and hooks to handle `loading` and `error` states. These are common ones that you will need to handle in your app.

Now let's go back to the template that exists and add a loading state.
In `src/components/TodoPrivateList.vue`, let's make the following modifications.

```javascript
<template>
  <div>
+   <div v-if="$apollo.queries.todos.loading">Loading...</div>
    <div class="todoListwrapper">
      <TodoItem 
        v-bind:todos="filteredTodos" 
        v-bind:type="type" 
      />
    </div>
    <TodoFilters 
      v-bind:remainingTodos="remainingTodos" 
      v-bind:filterResults="filterResults" 
      v-bind:filterType="filterType"
      v-bind:type="type"
      v-bind:clearCompleted="clearCompleted"
    />
  </div>
</template>
```

When this component mounts, the GraphQL query sent in the background may not have been completed. But we need to handle that temporary state of no data and hence we return some useful text during `loading` state. 
In this loading state, typically you can do fancy things like displaying a loading spinner.

Now, the query could also end up in an `error` state due to various reasons. Sometimes the graphql query could be wrong, or the server isn't responding. Whatever may be the reason, the user facing UI should show something to convey that an error has occurred. 
In this error state, typically you can send these error messages to third-party services to track what went wrong.

So let's add some code to handle errors. We will start off by defining a data property called `error` to store the error messages, if any.

```javascript
export default {
  components: {
    TodoItem, TodoFilters
  },
  data() {
    return {
      type: "private",
      filterType: "all",
      todos: [],
+     error: null
    }
  },
```

Now let's update the `apollo` definition to listen to the errors in the hook.

```javascript
  apollo: {
    todos: {
      // graphql query
      query: GET_MY_TODOS,
+     error(error) {
+       this.error = JSON.stringify(error.message);
+     }
    },
  },

```

So once an error occurs, we store the `error.message` value in our `error` data key. Finally let's update the html to handle this.

```javascript
<template>
  <div>
    <div v-if="$apollo.queries.todos.loading">Loading...</div>
+   <div v-if="error">{{ error }}</div>
    <div class="todoListwrapper">
      <TodoItem 
        v-bind:todos="filteredTodos" 
        v-bind:type="type" 
      />
    </div>
    <TodoFilters 
      v-bind:remainingTodos="remainingTodos" 
      v-bind:filterResults="filterResults" 
      v-bind:filterType="filterType"
      v-bind:type="type"
      v-bind:clearCompleted="clearCompleted"
    />
  </div>
</template>
```

Of course this is just a basic implementation of error handling. Now depending upon the error codes the server returns, you can show different messages to the user.

All said and done, these are two important states that need to be handled inside your component. What you have written above is basic, but sufficient for this tutorial.
