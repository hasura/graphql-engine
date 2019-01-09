# Auth setup

In the previous section, we saw how to create an instance of Apollo client and we wrote and exported a function that takes a JWT and returns an Apollo Client with JWT embedded.


> We will not go into the subtleties of client side authentication in this tutorial. Refer to [this example](https://github.com/expo/auth0-example) if you wish to see how auth has been implemented. 

Points to note before proceeding:

- After authenticaton, the core todo app starts at `src/navigation/DrawerNavigator`.
- By the time the App reaches `src/navigation/DrawerNavigator`, the following object already exists in `AsyncStorage` (the local storage for React Native apps).

  ```js
  {
    token: "<jwt>",
    name: "<username>",
    id: "<auth0-user-id>",
    exp: "<JWT expiry time>"
  }
  ```

In this section we will do this following:

1. Create an Apollo client with JWT
2. Make a mutation to the `users` table to insert the `name` and `auth0_id` of the user so that other data can be related to the users.
3. Provide the app with Apollo Client using `ApolloProvider` from `react-apollo`. This is necessary to use the react-apollo components like `<Query>`, `<Mutation>`, `<Subscription>`  etc.


## Creating Apollo Client with JWT

As you know, we wrote a function called `makeApolloClient` in the previous section. Let us import it and instantiate the client in `componentDidMount` of `DrawerNavigator`. 

```js
import makeApolloClient from '../../apollo';

export class DrawerNav extends React.Component {
  ...
  async componentDidMount() {
    // fetch local session and parse it
    const session = await AsyncStorage.getItem('@todo-graphql:auth0');
    const sessionObj = JSON.parse(session);
    const { token, id, name } = sessionObj;
    
    // make apollo client with this session token
    const client = await makeApolloClient(token);
  } 
  ...
}

```

## Insert user information in database

Next, we have to insert the user information in the `users` table so that rest of the data can be related to the user. Inserting to the `users` table is done in the form of a GraphQL mutation called `insert_users`. The mutation is:

```graphql
mutation ($auth0_id: String!, $name: String!) {
  insert_users (
    objects: [
      {
        name: $name,
        auth0_id:$auth0_id
      }
    ],
    on_conflict: {
      constraint: users_pkey,
      update_columns: [auth0_id, name]
    }
  ) {
    affected_rows
  }
}
```

In the above mutation, we insert the object:

```json
{
  "name": "$name",
  "auth0_id": "$auth0_id0"
}
```

into the `users` table where `$name` and `$auth0_id` come from the query variables.

> We have also used the `on_conflict` clause because the user might already be signed up

Apollo client has a public function called `mutate` that you can use for directly firing mutations. (More about this [here](../apollo-concepts/apollo-manually-making-queries.md))

Let us do this as well in the `componentDidMount` of `DrawerNavigator` and then set the `loading` state to `false` so that the children components are rendered.

Firstly, import `gql` (more about `gql` here):

```js
import gql from 'graphql-tag';
```

Then append the following to `componentDidMount`:

```js
await client.mutate({
  mutation: gql`
    mutation ($auth0_id: String!, $name: String!) {
      insert_users (
        objects: [
          {
            name: $name,
            auth0_id:$auth0_id
          }
        ],
        on_conflict: {
          constraint: users_pkey,
          update_columns: [auth0_id, name]
        }
      ) {
        affected_rows
      }
    }
  `,
  variables:variables: { auth0_id: id, name }
});

this.setState({
  client,
  loading: false
})
```

## Provide client to the app using ApolloProvider

Firstly, import `ApolloProvider`.

```js
import { ApolloProvider } from 'react-apollo';
```

In the render method of this component, Provider the `Drawer` with `<ApolloProvider>`. The render method should look like:

```js
render() {
  if (this.state.loading) {
    return <CenterSpinner />
  }
  // provide Apollo client to the entire app using ApolloProvider
  return (
    <ApolloProvider client={this.state.client}>
      <Drawer
        screenProps={{rootNavigation: this.props.screenProps.rootNavigation}}
      />
    </ApolloProvider>
}
```

## Wrapping up

Finally, your `src/navigation/DrawerNavigator` should provide the apollo client created with the token.

In the next section, we will integrate the current UI with the backend.
