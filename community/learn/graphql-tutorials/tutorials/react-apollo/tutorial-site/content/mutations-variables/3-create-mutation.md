---
title: "Run mutation, update cache"
metaTitle: "Apollo Mutation Component | GraphQL React Apollo Tutorial"
metaDescription: "We will use the Apollo Client Mutation component from react-apollo in React app as an example to insert new data and update cache locally using readQuery and writeQuery."
---

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/ZgMblvlIg28" />

### Apollo React Mutation Component
Now let's do the integration part. Open `src/components/Todo/TodoInput.js` and add the following code below the other imports:

```javscript
import { Mutation } from "react-apollo";
```

We are importing the `Mutation` component from `react-apollo` and the graphql query we defined above to fetch the todo data.

Now, we will wrap the component with `Mutation` passing our graphql mutation constant that we imported. Add the following code:

```javascript
const TodoInput = ({isPublic=false}) => {
+ return (
+  <Mutation mutation={ADD_TODO}>
+    {(addTodo, { loading, data }) => {
      return (
        <form className="formInput" onSubmit={(e) => {
          e.preventDefault();
        }}>
          <input
            className="input"
            placeholder="What needs to be done?"
          />
          <i className="inputMarker fa fa-angle-right" />
        </form>
      );
+    }}
+  </Mutation>
+ );
};
```

In the `<Mutation>` component defined above, the first argument of the render prop function is the mutate function; (addTodo) in this case. Read more about the mutate function [here](https://www.apollographql.com/docs/react/essentials/mutations.html#render-prop)

The mutate function optionally takes variables, optimisticResponse, refetchQueries, and update; You are going to make use of the `update` function later.

We'll get back to what the render props do a little later below. 

We need to handle the change event so that when the user types something on the input box, we update the state.

We are going to make use of `useState` hook for this.

```javascript
- import React from 'react';
+ import React, {useState} from 'react';
```

We will initialise the state and add an `onChange` handler to update the state.

```javascript
const TodoInput = ({isPublic = false}) => {
+  let input;

+  const [todoInput, setTodoInput] = useState('');
   return (
     <Mutation mutation={ADD_TODO}>
       {(addTodo, {loading, data}) => {
         return (
           <form className="formInput" onSubmit={(e) => {
             e.preventDefault();
           }}>
             <input
               className="input"
               placeholder="What needs to be done?"
+              value={todoInput}
+              onChange={e => (setTodoInput(e.target.value))}
+              ref={n => (input = n)}
             />
             <i className="inputMarker fa fa-angle-right" />
           </form>
         )
       }}
     </Mutation>
   );
};
```

Now let's handle the form submit to invoke the mutation.

```javascript
  <Mutation mutation={ADD_TODO}>
    {(addTodo, {loading, data}) => {
      return (
        <form className="formInput" onSubmit={(e) => {
          e.preventDefault();
+         addTodo({variables: {todo: todoInput, isPublic }});
        }}>
          <input
            className="input"
            placeholder="What needs to be done?"
            value={todoInput}
            onChange={e => (setTodoInput(e.target.value))}
            ref={n => (input = n)}
          />
          <i className="inputMarker fa fa-angle-right" />
        </form>
      )
    }}
  </Mutation>
```

We are passing the mutate function (`addTodo`) to our form submit handler.
The mutate function's first argument would be the mutation query's options, such as variables etc. We are now passing the variables required for the mutation. 

The mutation has been integrated and the new todos will be inserted into the database. But the UI doesn't know that a new todo has been added. We need a way to tell Apollo Client to update the query for the list of todos.

### Apollo React Mutation Update
The `update` function comes in handy to update the cache for this mutation. It comes with utility functions such as `readQuery` and `writeQuery` that helps in reading from and writing to the cache.

Let's implement `update` for the above mutation.

We pass the update function as a prop.

```javascript
-    <Mutation mutation={ADD_TODO}>
+    <Mutation mutation={ADD_TODO} update={updateCache}>
```

We need to fetch the current list of todos from the cache. So let's import the query that we used in the previous steps.

```javascript
import {GET_MY_TODOS} from './TodoPrivateList';
```

Let's define the updateCache function to read and write to cache.

```javascript
const TodoInput = ({isPublic = false}) => {
  let input;

  const [todoInput, setTodoInput] = useState('');
+  const updateCache = (cache, {data}) => {
+    // If this is for the public feed, do nothing
+    if (isPublic) {
+      return null;
+    }
+
+    // Fetch the todos from the cache
+    const existingTodos = cache.readQuery({
+      query: GET_MY_TODOS
+    });
+
+    // Add the new todo to the cache
+    const newTodo = data.insert_todos.returning[0];
+    cache.writeQuery({
+      query: GET_MY_TODOS,
+      data: {todos: [newTodo, ...existingTodos.todos]}
+    });
+  };
   return (
    ...
   );
};
```

Let's dissect what's happening in this code snippet.

Our goals were simple:

- Make a mutation to insert the new todo in the database.
- Once the mutation is done, we need to update the cache to update the UI.

The update function is used to update the cache after a mutation occurs.
It receives the result of the mutation (data) and the current cache (store) as arguments. You will then use these arguments to manage your cache so that the UI will be up to date.

### readQuery and writeQuery

cache.readQuery
---------------

Unlike `client.query`, readQuery will never make a request to your GraphQL server. It will always read from the cache. So we make a read request to the cache to get the current list of todos.

cache.writeQuery
----------------

We have already done the mutation to the graphql server using the mutate function. Our goal was to update the UI. This is where writeQuery comes to the rescue. writeQuery will allow you to change data in your local cache, but it is important to remember that they will not change any data on your server (exactly what we need).

  Any subscriber to the Apollo Client store will instantly see this update and render new UI accordingly.

We concatenate our new todo from our mutation with the list of existing todos and write the query back to the cache with cache.writeQuery

Now, the TodoPrivateList component wrapped with the `Query` component will get the updated todo list as it is automatically subscribed to the store.

Great! That was actually easy :)

Let's wrap this by adding a function to clear the input value once the mutation is successful.

```javascript
-  <Mutation mutation={ADD_TODO} update={updateCache}>
+  <Mutation mutation={ADD_TODO} update={updateCache} onCompleted={resetInput}>
```

We pass a function called `resetInput` to the `onCompleted` prop which will be called once the mutation is completed. The function definition looks like this:

```javascript
const TodoInput = ({isPublic = false}) => {
  let input;

  const [todoInput, setTodoInput] = useState('');

  const updateCache = (cache, {data}) => {
    ...
  };

+  const resetInput = () => {
+    setTodoInput('');
+    input.focus();
+  };

  return (
    ...
  );
}
```

