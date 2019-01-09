# Apollo Subscription Components

The Apollo `<Subscription>` components take `subscription` and `variables` as props and provide a render prop function that is executed and rendered everytime a subscription event happens.

```js
const ReactComponent = () => (
  <Subscription
    subscription={onlineUsers}
  >
    {
      ({data, loading, error }) => {
        if (loading) { return <Loading />; }
        if (error) { return <Error />; }
        return <RenderUsers users={data.users} />
      }
    }
  </Subscription>
)
```

In the above component, the Subscription component

1. Sets `loading=true` and makes the GraphQL query. Till it gets the response, it calls the render prop function with the following payload

    ```js
    {
      data: null,
      loading: true,
      error: null
    }
    ```

   ... gets `<Loading />` in return and therefore renders `<Loading />`

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


4. On every new event, the component is re-rendered with the new data.