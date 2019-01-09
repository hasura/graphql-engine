# Emit online events

In the previous section, we rendered the list of current online users but we have not implemented the logic to show the current user in the online list.

To do that, we need to make a mutation to the `users` table every five seconds and update the `last_seen` field. This is best done whenever the user logs in.

Go to  `src/navigation/DrawerNavigator.js` and emit online events every 5 seconds from the `componentDidMount` function.

The mutation you need to do is:

```js
const emitOnlineEvent = gql`
mutation ($userId: String) {
  update_users(
    _set: {
      last_seen: "now()"
    },
    where: {
      auth0_id: {
        _eq: $userId
      }
    }
  ) {
    affected_rows
  }
}
`
```

Emit the above mutation using the `client.mutate` function every five seconds using the JS `setInterval`  function.

```js
// start emitting events saying that the useri s online
setInterval(
  () => client.mutate({
    mutation: emitOnlineEvent,
    variables: {
      userId: id
    }
  }),
  5000
);
```

## Wrapping up

After you do the above, your `componentDidMount` in `src/navigation/DrawerNavigator.js` should look something like:

```js
async componentDidMount() {
    // fetch session
    const session = await AsyncStorage.getItem('@todo-graphql:auth0');
    const sessionObj = JSON.parse(session);
    const { token, id, name } = sessionObj;
    // make apollo client with this session token
    const client = await makeApolloClient(token);
    // insert the user into user table
    const resp = await client.mutate({
      mutation: insertUser,
      variables: {
        auth0_id: id,
        name
      }
    });
    // set loading to false
    if (resp.data) {
      this.setState({
        client,
        loading: false
      })
    }

    // start emitting events saying that the useri s online
    setInterval(
      () => client.mutate({
        mutation: emitOnlineEvent,
        variables: {
          userId: id
        }
      }),
      5000
    );
}
```

In the next section, we will look at building a realtime feed for the Public todos.