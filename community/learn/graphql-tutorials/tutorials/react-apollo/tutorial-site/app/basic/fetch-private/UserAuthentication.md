Now that you have Apollo client set up, you need to create the user.

Open `src/routes.js` and update the following code:

```
const handleAuthentication = ({ location }) => {
  if (/access_token|id_token|error/.test(location.hash)) {
    auth.handleAuthentication(client); // we are now passing client
  }
};
```

Here we are just passing the apollo client instance to handleAuthentication method which is called after the user has logged in.

Open `src/components/Auth/Auth.js` and include this import:

```
import gql from "graphql-tag";
```

and update the `handleAuthentication` method to accept `client` as an argument.

```
  handleAuthentication = (client) => {
    ...rest of the code below
  }
```

Finally, add the logic to create/update the user after successful login. 

```
  handleAuthentication = (client) => {
    ...some boilerplate code
        this.auth0.client.userInfo(authResult.accessToken, function(err, user) {
          // Now you have the user's information
          client
            .mutate({
              mutation: gql`
                mutation($userId: String!, $nickname: String) {
                  insert_users(
                    objects: [{ auth0_id: $userId, name: $nickname }]
                    on_conflict: {
                      constraint: users_pkey
                      update_columns: [last_seen, name]
                    }
                  ) {
                    affected_rows
                  }
                }
              `,
              variables: {
                userId: user.sub,
                nickname: user.nickname
              }
            })
            .then(() => {
              window.location.href = "/home";
            })
            .catch(error => {
              console.error(error);
              // alert(JSON.stringify(error));
            });
          window.location.href = "/home";
        });
      } 
      ...some other code
    });
  };
```

Remember we learnt that Apollo Client can be used in two ways to send a query. `client.query` or `Query` component. Similarly for mutation, we are using the `client.mutate` method to insert the user into the database. 

It is totally okay to be overwhelmed with the above code. We will come back to this later in detail. For now, we have ensured that the new user who is logged in, is registered as a user and stored in our database using the GraphQL mutation.

