# Apollo custom queries and mutations

Sometimes the Apollo Queries and Mutation components might not be sufficient for your uses case and you might want to manually fire queries. You can do that using the client that you can access using `ApolloConsumer` from `react-apollo`.

```js
const Comp = () => (
  <ApolloConsumer>
    {
      client => {
        return <ChildComponent client={client} />
      }
    }
  </ApolloConsumer>
);
```


In our todo app we eventually want to fetch older and newer todos and we will need the client for that. For that case, in `src/screens/TodoScreen.js`, we must wrap the `Todos` component inside `ApolloConsumer` and pass the client as a prop. Modify the render method of the `TodoScreen` component as follows:

```js
render() {
    // other stuff
    return (
      <View style={styles.container}>
        <Textbox
          isPublic={this.props.isPublic}
          navigate={this.props.navigate}
          userId={this.state.id}
          username={this.state.name}
          token={this.state.token}
        />
        <ApolloConsumer>
          {
            client => (
              <View
                style={styles.todoListContainer}
              > 
                <Todos
                  isPublic={this.props.isPublic}
                  navigate={this.props.navigate}
                  userId={this.state.id}
                  username={this.state.name}
                  token={this.state.token}
                  client={client}
                />
              </View>
            )
          }
          
        </ApolloConsumer>
      </View>
    );
  )
}
```

Now our `Todos` component has the `client` prop and we can make queries, mutations and subscriptions as follows.

## Queries

```js
const { data } = await client.query({
  query: gql`query ($var: Int) { queryField (var: $var) { field1 field2 }}
  variables: { var: 25}
});
```

## Mutation

```js
const { data } = await client.mutate({
  mutation: gql`mutation ($var: Int) { mutationField (var: $var) { field1 field2 }}
  variables: { var: 25}
});
```

## Subscription

```js
client.subscribe({
  query: gql`query ($var: Int) { queryField (var: $var) { field1 field2 }}
  variables: { var: 25} 
}).subscribe({
    next: (event) => {
      handleEvent(event);
    },
    error: (err) => {
      console.error("err", err);
    }
  })
}
```


We will use `client.query` in the Todos component for fetching newer and older todos. We will use the `client.subscribe` field for getting realtime updates in our public todos feed.

## Wrapping up

Now you have learned how to manually fire queries, mutations and subscriptions using Apollo Client. 

In the next section, we will see how to manipulate the Apollo cache for updating the UI.
