The functionality for the notification banner is simple.

Whenever the user clicks on the banner, we need to load the new data from the server and append it to the local public todo list.

Now we need to define the query to fetch new data from the server.

Open `src/components/Todo/TodoQueries.js` and add the following:

```
const QUERY_FEED_PUBLIC_TODO = gql`
  query fetch_todos($todoId: Int) {
    todos(
      where: { is_public: { _eq: true }, id: { _gt: $todoId } }
      order_by: { created_at: desc }
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

export {
    QUERY_FEED_PUBLIC_TODO
}
```

Now that the query is ready, import it along with other queries which we had previously imported.

```
import {
  QUERY_FEED_PUBLIC_TODO,
} from "./TodoQueries";
```

Open `src/components/Todo/TodoPublicList.js` and add the following code below componentDidMount.

```
  loadMoreClicked() {
    const { client } = this.props;
    this.setState({ ...this.state, showNew: false, newTodosLength: 0 });
    client
      .query({
        query: QUERY_FEED_PUBLIC_TODO,
        variables: {
          todoId: this.state.todos.length ? this.state.todos[0].id : null
        }
      })
      .then(data => {
        if (data.data.todos.length) {
          const mergedTodos = data.data.todos.concat(this.state.todos);
          // update state with new todos
          this.setState({ ...this.state, todos: mergedTodos });
        }
      });
  }
```

Update the constructor to bind the `loadMoreClicked` method with `this`.

```
    this.loadMoreClicked = this.loadMoreClicked.bind(this);
```

Once we get the response from `client.query`, we update the local react state by merging the new todos with the already available public todo list.

Once you update the state, the component will re-render and update the UI to reflect the latest public todo data.

Open the UI and try out the above implementation

<insert image>


---

Now we need to define the query to fetch old data from the server.

Open `src/components/Todo/TodoQueries.js` and add the following:

```
const QUERY_FEED_PUBLIC_OLD_TODO = gql`
  query fetch_todos($todoId: Int) {
    todos(
      where: { is_public: { _eq: true }, id: { _lt: $todoId } }
      limit: 5
      order_by: { created_at: desc }
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

export {
    QUERY_FEED_PUBLIC_OLD_TODO
}
```

Now that the query is ready, import it along with other queries which we had previously imported.

```
import {
  QUERY_FEED_PUBLIC_OLD_TODO,
} from "./TodoQueries";
```

Open `src/components/Todo/TodoPublicList.js` and add the following code below loadMoreClicked method

```
  loadOlderClicked() {
    const { client } = this.props;
    client
      .query({
        query: QUERY_FEED_PUBLIC_OLD_TODO,
        variables: {
          todoId: this.state.todos.length
            ? this.state.todos[this.state.todos.length - 1].id
            : null
        }
      })
      .then(data => {
        if (data.data.todos.length) {
          const mergedTodos = this.state.todos.concat(data.data.todos);
          // update state with new todos
          this.setState({ ...this.state, todos: mergedTodos });
        } else {
          this.setState({ ...this.state, showOlder: false });
        }
      });
  }
```

Update the constructor to bind the `loadOlderClicked` method with `this`.

```
    this.loadOlderClicked = this.loadOlderClicked.bind(this);
```

Again, after we get the response from `client.query`, we update the local react state by merging the new todos with the already available public todo list. This time we concat it at the end to display the old todos in the end.

Once you update the state, the component will re-render and update the UI to reflect the old public todo data.








