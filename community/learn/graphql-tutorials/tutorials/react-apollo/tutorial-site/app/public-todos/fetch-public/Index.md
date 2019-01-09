Let's define the graphql query to be used:

Open `src/components/Todo/TodoQueries.js` and add the following code:

```
const QUERY_PUBLIC_TODO = gql`
  query fetch_todos($todoLimit: Int, $todoId: Int) {
    todos(
      where: { is_public: { _eq: true }, id: { _gt: $todoId } }
      order_by: { created_at: desc }
      limit: $todoLimit
    ) {
      id
      text
      is_completed
      created_at
      is_public
      user {
        name
      }
    }
  }
`;
const SUBSCRIPTION_TODO_PUBLIC_LIST = gql`
  subscription($todoId: Int) {
    todos(
      where: { is_public: { _eq: true }, id: { _gt: $todoId } }
      order_by: { created_at: desc }
      limit: 1
    ) {
      id
      text
      is_completed
      created_at
      is_public
    }
  }
`;
export { 
  QUERY_PUBLIC_TODO,
  SUBSCRIPTION_TODO_PUBLIC_LIST
};
```

What does the query do? 
-----------------------
The query fetches `todos` with a simple condition; `is_public` must be true and must be greater than the todoId that i already have. We also limit the number of todos using a variable.
We sort the todos descending by its created_at time according to the schema. We specify which fields we need for the todos node.

Nested GraphQL Query
--------------------
Note that our query to fetch todos, also has a nested object to fetch user information.

```
    {
        id 
        text
        ...
        user {
          name
        }
    }
```

This is the power of GraphQL. You can specify any level of nesting your server supports to query exactly the data that you require without making multiple roundtrips to the server. Imagine making multiple REST API calls to get this data.

What does the subscription do?
------------------------------
Similar to the query, the subscription fetches `todos` as soon as something new is available.

Great! The query and subscription is now ready, let's integrate it with our react code.

Open `src/components/Todo/TodoPublicWrapper.js` and modify the `<TodoPublicList>` to additionally pass the client prop. We will use this to make query and subscribe calls.

```
<TodoPublicList userId={userId} type="public" client={this.props.client} />
```

Now open `src/components/Todo/TodoPublicList.js` and add the following code below the other imports:

```
import {
  SUBSCRIPTION_TODO_PUBLIC_LIST,
  QUERY_PUBLIC_TODO,
} from "./TodoQueries";
```

We are going to make use of `client.query` and `client.subscribe` inside componentDidMount of this component.

`client.query` is executed to fetch the list of public todos, (the query was defined above as QUERY_PUBLIC_TODO)

`client.subscribe` is executed to listen on new changes to todos. (the query was defined above as SUBSCRIPTION_TODO_PUBLIC_LIST)

```
  componentDidMount() {
    const { client } = this.props;
    const _this = this;
    // query for public todos
    client
      .query({
        query: QUERY_PUBLIC_TODO,
        variables: { todoLimit: this.state.limit }
      })
      .then(data => {
        this.setState({ todos: data.data.todos });
        const latestTodoId = data.data.todos.length
          ? data.data.todos[0].id
          : null;
        // start a subscription
        client
          .subscribe({
            query: SUBSCRIPTION_TODO_PUBLIC_LIST,
            variables: { todoId: latestTodoId } // update subscription when todoId changes
          })
          .subscribe({
            next(data) {
              if (data.data.todos.length) {
                _this.setState({
                  ...this.state,
                  showNew: true,
                  newTodosLength:
                    _this.state.newTodosLength + data.data.todos.length
                });
              }
            },
            error(err) {
              console.error("err", err);
            }
          });
      });
  }
```

What is the logic?
------------------
Whenever the react component mounts, componentDidMount lifecycle method will be called. We are making a query inside that to fetch the list of todos using `client.query`. Once we get the response for initial set of todos, we update the local react state to display the fetched todos. This will trigger a re-render of the component.

We then make a `client.subscribe` call to fetch todos which are greater than the latest fetched todo in the public todo list. We pass that as a variable.

Once new subscription data comes in, we update the local react state using `this.setState` 

Note that, we have already written the react logic to display the todo data. We can now remove the mock state for todos, by initialising it to empty array in the constructor.

```
    this.state = { 
        ...other values
        todos: [] // remove mock data
    }
```
