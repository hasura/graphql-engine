---
title: "Subscriptions to show online users"
metaTitle: "Update last seen of user with Mutation | GraphQL React Apollo Tutorial"
metaDescription: "GraphQL Mutation to update last seen of user to make them available online. Use setInterval to trigger mutation every few seconds "
---

import GithubLink from "../src/GithubLink.js";
import YoutubeEmbed from "../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/Zegh4VJqrHM" />

We cruised through our GraphQL queries and mutations. We queried for todos, added a new todo, updated an existing todo, removed an existing todo.

Now let's get to the exciting part.

GraphQL Subscriptions
---------------------

We have a section of UI which displays the list of online users. So far we have made queries to fetch data and display them on the UI. But typically online users data is dynamic.

We can make use of GraphQL Subscription API to get realtime data from the graphql server to efficiently handle this.

But but but...

We need to tell the server that the user who is logged in is online. We have to poll our server to do a mutation which updates the `last_seen` timestamp value of the user.

We have to make this change to see yourself online first. Remember that you are already logged in, registered your data in the server, but not updated your `last_seen` value.?

The goal is to update every few seconds from the client that you are online. Ideally you should do this after you have successfully authenticated with Auth0. So let's update some code to handle this. 

Open `src/components/OnlineUsers/OnlineUsersWrapper.js` and add the following imports and set the client prop in the constructor

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/react-apollo/app-final/src/components/OnlineUsers/OnlineUsersWrapper.js" text="src/components/OnlineUsers/OnlineUsersWrapper.js" />

```javascript
+ import gql from "graphql-tag";
+ import {withApollo} from 'react-apollo';
class OnlineUsersWrapper extends Component {
- constructor() {
+ constructor(props) {
-   super();
+   super(props);
+   this.client = props.client;

    this.state = {
      onlineUsers: [
        { name: "someUser1" },
        { name: "someUser2" }
      ]
    };
  }
```

Update the export by wrapping the OnlineUsersWrapper component with `withApollo`

```javascript
- export default OnlineUsersWrapper;
+ export default withApollo(OnlineUsersWrapper);
```

In `componentDidMount`, we will create a `setInterval` to update the last_seen of the user every 30 seconds.

```javascript
class OnlineUsersWrapper extends Component {
  constructor(props) {
    super(props);
    this.client = props.client;
  }
+ componentDidMount() {
+   // Every 30s, run a mutation to tell the backend that you're online
+   this.onlineIndicator = setInterval(() => this.updateLastSeen(), 30000);
+ }
```

Now let's write the definition of the `updateLastSeen`.

```javascript
class OnlineUsersWrapper extends Component {
  constructor(props) {
    super(props);
    this.client = props.client;
  }
+  updateLastSeen() {
+    // Use the apollo client to run a mutation to update the last_seen value
+    const UPDATE_LASTSEEN_MUTATION=gql`
+      mutation updateLastSeen ($now: timestamptz!) {
+        update_users(where: {}, _set: {last_seen: $now}) {
+          affected_rows
+        }
+      }`;
+    this.client.mutate({
+      mutation: UPDATE_LASTSEEN_MUTATION,
+      variables: {now: (new Date()).toISOString()}
+    });
+  }
  componentDidMount() {
    // Every 30s, run a mutation to tell the backend that you're online
    this.onlineIndicator = setInterval(() => this.updateLastSeen(), 30000);
  }
```

Again, we are making use of `client.mutate` to update the `users` table of the database.

Great! Now the metadata about whether the user is online will be available in the backend. Let's now do the integration to display realtime data of online users.
