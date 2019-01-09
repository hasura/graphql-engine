Open `src/components/Todo/TodoInput.js` and add the following code below the other imports:

```
import { Mutation } from "react-apollo";
import {
  MUTATION_TODO_ADD
} from "./TodoQueries";

```

We are importing the `Mutation` component from `react-apollo` and the graphql query we defined above to fetch the todo data.

Now, we will wrap the component with `Mutation` passing our graphql mutation constant that we imported. Replace the `return` with the following code:

```
  <Mutation mutation={MUTATION_TODO_ADD}>
    {(addTodo, { error }) => {
      if (error) {
        alert("Something went wrong");
      }
      return (
        // paste your original return of this component.
      );
    }}
  </Mutation>
```

We'll get back to what the render props do a little later below. 

We need to handle the keypress event so that when user presses Enter, we capture that event and make a mutation. In the input element, attach the `onKeyPress` event handler. 

```
    <input
      ...
      ...
      onKeyPress={e => {
        this.handleTextboxKeyPress(e, addTodo);
      }}
    />
```

In the constructor, add the binding for the method as following:

```
  this.handleTextboxKeyPress = this.handleTextboxKeyPress.bind(this);
```

This is done to get the context of `this` inside the method.

Now update the imports as follows:

```
import {
  QUERY_PRIVATE_TODO,
  MUTATION_TODO_ADD
} from "./TodoQueries";
```

We are adding `QUERY_PRIVATE_TODO` to use with the store API. Now add the method definition code just above the `render` method.

```
  handleTextboxKeyPress(e, addTodo) {
    if (e.key === "Enter") {
      const newTodo = this.state.textboxValue;
      const userId = this.props.userId;
      const isPublic = this.props.type === "public" ? true : false;
      addTodo({
        variables: {
          objects: [
            {
              text: newTodo,
              user_id: userId,
              is_completed: false,
              is_public: isPublic
            }
          ]
        },
        update: (store, { data: { insert_todos } }) => {
          const query = QUERY_PRIVATE_TODO;
          try {
            if (this.props.type === "private") {
              const data = store.readQuery({
                query: query,
                variables: { userId: this.props.userId }
              });
              const insertedTodo = insert_todos.returning;
              data.todos.splice(0, 0, insertedTodo[0]);
              store.writeQuery({
                query: query,
                variables: {
                  userId: this.props.userId
                },
                data
              });
            }
          } catch (e) {
            console.error(e);
          }
          this.setState({
            ...this.state,
            textboxValue: ""
          });
        }
      });
    }
  }
```

Let's dissect what's happening in this code snippet.

Our goals were simple: 

- Make a mutation to insert the new todo in the database.
- Once the mutation is done, we need to update the cache to update the UI.

In the `<Mutation>` component defined above, the first argument of the render prop function is the mutate function; (addTodo) in this case. Read more about the mutate function [here](https://www.apollographql.com/docs/react/essentials/mutations.html#render-prop)

The mutate function optionally takes variables, optimisticResponse, refetchQueries, and update; You are going to make use of the `update` function.

The update function is used to update the cache after a mutation occurs. 
It receives the result of the mutation (data) and the current cache (store) as arguments. You will then use these arguments to manage your cache so that the UI will be up to date.

We are passing the mutate function (`addTodo`) to our onKeyPress handler.
The mutate function's first argument would be the mutation query's options, such as variables etc. We are now passing the variables required for the mutation. 

As we know that our `MUTATION_TODO_ADD` that was defined, accepted query variables called `objects`. So we are now passing the new todo json object as the value of the variable. Once this is done, the actual mutation to the graphql server is complete.

It makes sense now! But the job is not complete. Though the mutation is complete, we have to update our UI. So we make use of the `store` API for updating the cache. As soon as the mutation is done, the `update` function is called where you have the store and mutation response as arguments.

We make use of the `store.readQuery` and `store.writeQuery` methods.

store.readQuery
---------------

Unlike `client.query`, readQuery will never make a request to your GraphQL server. It will always read from the cache. So we make a read request to the cache to get the current list of todos.

store.writeQuery
----------------

We have already done the mutation to the graphql server using the mutate function. Our goal was to update the UI. This is where writeQuery comes for the rescue. writeQuery will allow you to change data in your local cache, but it is important to remember that they will not change any data on your server (exactly what we need).

  Any subscriber to the Apollo Client store will instantly see this update and render new UI accordingly.

Now, the TodoPrivateList component wrapped with the `Query` component will get the updated todo list as it is automatically subscribed to the store.

Great! That was actually easy :)

