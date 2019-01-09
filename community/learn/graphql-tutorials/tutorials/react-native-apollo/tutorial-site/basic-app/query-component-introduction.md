# Apollo Query Components

The Apollo `<Query>` components take `query` and `variables` as props and provide a render prop function that is executed and rendered after the data has been fetched using the GraphQL query. For example,

```js
import { Query } from 'react-apollo';

const ReactComponent = () => (
  <Query
    query={fetchTodos}
    variables={{ limit: 10, offset: 0}}
  >
    {
      ({data, loading, error }) => {
        if (loading) { return <Loading />; }
        if (error) { return <Error />; }
        return <RenderTodos todos={data.todos} />
      }
    }
  </Query>
)
```

In the above component, the `<Query>` component follows this flow:

1. Sets `loading=true` and makes the GraphQL query. Till it gets the response, it calls the render prop function with the following payload

    ```js
    {
      data: null,
      loading: true,
      error: null
    }
    ```

   and gets `<Loading />` in return and therefore renders `<Loading />`

2. If the query proceeds successfully, it sets `loading=false` and renders the output of the render prop function when called with

    ```js
    {
      data: { todos: [ ... ] }
      loading: false,
      error: null
    }
    ```

    which essentially is `<RenderTodos todos={data.todos}/>`.

3. If there is an error while making the query, it sets `loading=false` and renders the output of the render prop function when called with

    ```js
    {
      data: null,
      loading: false,
      error: { ... }
    }
    ```

    which essentially is `<Error />`.


The `<Query>` component provides more functions like `fetchMore`, `refetch` as arguments in the render prop that we can use for pagination, refetching etc. See more [here](https://www.apollographql.com/docs/react/essentials/queries.html).