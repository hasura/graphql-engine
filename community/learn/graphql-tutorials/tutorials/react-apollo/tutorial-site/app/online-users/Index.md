We cruised through our first GraphQL query and mutation. We queried for todos and added a todo.

Now let's get to the exciting part.

GraphQL Subscriptions
---------------------

We have a section of UI which displays the list of online users. So far we have made queries to fetch data and display them on the UI. But typically online users data is dynamic. 

So do we poll our server every few seconds with a graphql query to see who are online? Technically, you can do it. But it will put a huge load on the server when all of your clients are requesting it at the same time.

We can make use of GraphQL Subscription API to get realtime data from the graphql server to efficiently handle this.

But but but...

We need to tell the server that the user who is logged in is online. We have to poll our server to do a mutation which updates the `last_seen` timestamp value of the user.

We have to make this change to see yourself online first. Remember that you are already logged in, registered your data in the server, but not updated your `last_seen` value.?

The goal is to update every few seconds from the client that you are online. Ideally you should do this after you have successfully authenticated with Auth0. So let's update some code to handle this. 

Open `src/components/Home/Home.js` and add the following imports:

```
import gql from "graphql-tag";
import moment from "moment";
```

In `componentDidMount`, we will create a `setInterval` to update the last_seen of the user every 5 seconds.

```
  componentDidMount() {
    // eslint-disable-next-line
    const lastSeenMutation = setInterval(this.updateLastSeen.bind(this), 5000);
  }
```

Now let's write the definition of the `updateLastSeen`.

```
  updateLastSeen = () => {
    const userId = localStorage.getItem("auth0:id_token:sub");
    const timestamp = moment().format();
    if (this.props.client) {
      this.props.client
        .mutate({
          mutation: gql`
            mutation($userId: String!, $timestamp: timestamptz!) {
              update_users(
                where: { auth0_id: { _eq: $userId } }
                _set: { auth0_id: $userId, last_seen: $timestamp }
              ) {
                affected_rows
              }
            }
          `,
          variables: {
            userId: userId,
            timestamp: timestamp
          }
        })
        .then(() => {
          // handle response if required
        })
        .catch(error => {
          console.error(error);
        });
    }
  };
```

Again, we are making use of `client.mutate` to update the `users` table of the database.

Great! Now the metadata about whether the user is online will be available in the backend. Let's now do the integration to display realtime data of online users.

