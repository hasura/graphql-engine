---
title: "Run mutation, update cache"
metaTitle: "Apollo Mutation with useMutation Hook | GraphQL React Apollo Typescript Tutorial"
metaDescription: "We will use the Apollo useMutation hook from @apollo/react-hooks in React app as an example to insert new data and update cache locally using readQuery and writeQuery."
---

### Apollo useMutation Hook
Now let's do the integration part. Open `src/components/Todo/TodoInput.tsx` and add the following code below the other imports:

```javscript
import { useMutation } from "@apollo/react-hooks";
```

We are importing the `useMutation` hook from `@apollo/react-hooks` and passing it the `ADD_TODO` mutation that we defined in the previous step.

```javascript

  const TodoInput = ({isPublic=false}) => {
    const [todoInput, setTodoInput] = React.useState('');
+   const [addTodo] = useMutation(ADD_TODO);
    return (

```

In the `useMutation` hook defined above, the first property of the result object is the mutate function; (addTodo) in this case. Read more about the mutate function [here](https://www.apollographql.com/docs/react/essentials/mutations.html)

The mutate function optionally takes variables and update among other arguments; You are going to make use of the `update` function later.

Now let's handle the form submit to invoke the mutation.

```javascript
  const TodoInput = ({isPublic=false}) => {
    const [todoInput, setTodoInput] = React.useState('');
    const [addTodo] = useMutation(ADD_TODO);
    return (
      <form className="formInput" onSubmit={(e) => {
        e.preventDefault();
        // add todo
+       addTodo(
+         {
+           variables: {todo: todoInput, isPublic }
+         }
+       );
      }}>
        <input
          className="input"
          placeholder="What needs to be done?"
          value={todoInput}
          onChange={e => (setTodoInput(e.target.value))}
        />
        <i className="inputMarker fa fa-angle-right" />
      </form>
    );
  };

```

We are passing the mutate function (`addTodo`) to our form submit handler.
The mutate function's first argument would be the mutation query's options, such as variables etc. We are now passing the variables required for the mutation. 

The mutation has been integrated and the new todos will be inserted into the database. But the UI doesn't know that a new todo has been added. If you add a todo and refresh the page you can see the changes. But we would like to do it without a page refresh. We will be using the cache for that.
We need a way to tell Apollo Client to update the query for the list of todos.

### Apollo React Mutation Update
The `update` function comes in handy to update the cache for this mutation. It comes with utility functions such as `readQuery` and `writeQuery` that helps in reading from and writing to the cache.

Let's implement `update` for the above mutation.

We need to fetch the current list of todos from the cache before adding the newly added todo. So let's import the query that we used in the previous steps.

```javascript
  import { useMutation } from "@apollo/react-hooks";
+ import { GET_MY_TODOS } from './TodoPrivateList';
```

Let's define the update function to read and write to cache.

```javascript

  const TodoInput = ({isPublic=false}) => {
    const [todoInput, setTodoInput] = React.useState('');
    const [addTodo] = useMutation(ADD_TODO);
    return (
      <form className="formInput" onSubmit={(e) => {
        e.preventDefault();
        // add todo
        addTodo({
-         variables: {todo: todoInput, isPublic }
+         variables: {todo: todoInput, isPublic },
+         update(cache, { data }) {
+           // do not update cache for public feed
+           if (isPublic || !data) {
+             return null;
+           }
+           const getExistingTodos : any = cache.readQuery({ query: GET_MY_TODOS });
+           // Add the new todo to the cache
+           const existingTodos = getExistingTodos ? getExistingTodos.todos : [];
+           const newTodo = data.insert_todos!.returning[0];
+           cache.writeQuery({
+             query: GET_MY_TODOS,
+             data: {todos: [newTodo, ...existingTodos]}
+           });
+         }
+       });
      }}>
        <input
          className="input"
          placeholder="What needs to be done?"
          value={todoInput}
          onChange={e => (setTodoInput(e.target.value))}
        />
        <i className="inputMarker fa fa-angle-right" />
      </form>
    );
  };

```

*Note*: We are using `any` type for `getExistingTodos` constant. We will get back to type mapping in the next step and update it accordingly.

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

Now, the TodoPrivateList component should get the updated todo list as it is automatically subscribed to the store.

Great! That was actually easy :)

Let's wrap this by updating the state to clear the input value once the mutation is completed.

```javascript

  <form className="formInput" onSubmit={(e) => {
    e.preventDefault();
    // add todo
    addTodo({
      ...
    });
+   setTodoInput('');
  }}>
```


