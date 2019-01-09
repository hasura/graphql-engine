# Apollo Mutation Components

The Apollo `<Mutation>` components take `mutation` and `variables` as props and provides a render prop that takes the has the `mutate` function as an arguement. This `mutate` function can be called on button click, on hover or whenever else required. For example,

It also takes a prop called `update` where we can manipulate the cache based on the results of the mutation in order to see instant UI updates.

```js
const ReactComponent = () => (
  <Mutation
    mutation={insertTodos}
    variables={{ ...variables }}
    update={(cache, {data: {insert_todos}}) => {
      // update cache for instant UI updates or do custom logic
      console.log('Mutation completed')
    }}
  >
    {
      (mutate, {data, loading, error }) => {
        return <Button onPress={mutate} title="Add Todo" disabled={loading} disabled={loading}/>
      }
    }
  </Mutation>
)
```

In the above component, when the button is pressed, the `<Mutation>` component sets `loading=true` and sends the mutation query to the database. Since the `disabled` prop of the button subscribes to the `loading` state, the button stays disabled till `loading` is set to false.

Similarly you can use the `error` state for custom logic.

When the mutation is completed, the `update` function is called which in this case logs `Mutation completed` on the console.

In the next two sections, we will use `<Mutation>` components to insert, update and delete todos in the app.
