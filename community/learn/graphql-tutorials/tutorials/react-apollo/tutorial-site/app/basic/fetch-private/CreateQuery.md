In this section, we will implement GraphQL Queries and integrate with the react UI.
With Apollo Client, you can send queries in 2 different ways.

1. Using the `query` method directly and then process the response as a promise.
2. New Render Prop API with React. (Recommended)

The recommended method is to use the render prop method, where you will just pass your GraphQL query as prop and `<Query />` component will fetch the data automatically and will present it in the component's render prop function.

Great! Now let's define the graphql query to be used:

Open `src/components/Todo/TodoQueries.js` and add the following code:

```
import gql from "graphql-tag";
const QUERY_PRIVATE_TODO = gql`
  query fetch_todos($userId: String!) {
    todos(
      where: { is_public: { _eq: false }, user_id: { _eq: $userId } }
      order_by: { created_at: desc }
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
  QUERY_PRIVATE_TODO
};
```

We have now written the graphql query as a javascript constant using the `gql` parser function. This function is used to parse the plain string as a graphql query.

What does this query do? 
------------------------
The query fetches `todos` with two conditions; `is_public` must be false and must be created by the logged in `user_id`. We sort the todos descending by its `created_at` time according to the schema. We specify which fields we need for the todos node.

[Try out](https://graphiql-online.com) this query now!

Great! The query is now ready, let's integrate it with our react code.

Open `src/components/Todo/TodoPrivateWrapper.js` and modify the `<TodoPrivateList>` to additionally pass the client prop.

```
<TodoPrivateList userId={userId} client={this.props.client} type="private" />
```

Remember that we wrapped our routes with `<ApolloProvider>` and passed `client` as a prop. We are using the same client prop to send it down to the components.

Now open `src/components/Todo/TodoPrivateList.js` and add the following code below the other imports:

```
import { Query } from "react-apollo";
import { QUERY_PRIVATE_TODO } from "./TodoQueries";
```

We are importing the `Query` component from `react-apollo` and the graphql query we defined above to fetch the todo data.

Let's remove the mock `data` constant which was used to populate sample data.

Now, we will wrap the component with `Query` passing our graphql query. replace the `return` with the following code:

```
  <Query query={QUERY_PRIVATE_TODO} variables={{ userId: userId }}>
    {({ loading, error, data, refetch }) => {
      if (loading) {
        return <div>Loading. Please wait...</div>;
      }
      if (error) {
        return <div>An error occurred</div>;
      }
      refetch();
      const finalData = data.todos;
      return (
        // paste your original return of this component.
      );
    }}
  </Query>

```

Woot! You have written your first GraphQL integration with React. Easy isn't it?

How does this work?
-------------------
When you wrapped your return with `<Query>` component, Apollo injected props into the component’s render prop function. Most important ones are:

`loading`: A boolean that indicates whether the request is in flight. If loading is true, then the request hasn't finished. Typically this information can be used to display a loading spinner.
`error`: A runtime error with graphQLErrors and networkError properties. Contains information about what went wrong with your query.
`data`: An object containing the result of your GraphQL query. This will contain our actual data from the server. In our case, it will be the todo data.

You can read more about other render props that Apollo passes [here](https://www.apollographql.com/docs/react/essentials/queries.html#render-prop)

Using the `data` prop, we are parsing the results from the server. In our query, `data` prop has an array `todos` which can be mapped over to render each `TodoItem`.

Now, lets apply some client side filtering to the todos that are displayed. Just before your `return` inside `<Query>` component, add the following code:

```

    let finalData = data.todos; // this has changed from const to let
    // apply filters for displaying todos
    if (this.state.filter === "active") {
      finalData = data.todos.filter(todo => todo.is_completed !== true);
    } else if (this.state.filter === "completed") {
      finalData = data.todos.filter(todo => todo.is_completed === true);
    }

```






