---
title: "Create Subscription and Render Result"
metaTitle: "Apollo Subscription Component | GraphQL React Apollo Tutorial"
metaDescription: "Integrate React Apollo Subscription Component to watch for changes in realtime data. We use GraphQL subscriptions as an example to get live data in the React app"
---

import GithubLink from "../../src/GithubLink.js";
import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/yqL_PpEAU7E" />

So let's define the graphql subscription to be used.

Open `src/components/OnlineUsers/OnlineUsersWrapper.js` and add the following code, below the other imports

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/react-apollo/app-final/src/OnlineUsers/OnlineUsersWrapper.js" text="src/OnlineUsers/OnlineUsersWrapper.js" />

```javascript
- import React, { Component } from "react";
+ import React, { Component, Fragment } from "react";
- import {withApollo} from 'react-apollo';
+ import {withApollo, Subscription} from 'react-apollo';
```

We are importing the `Subscription` component from `react-apollo` and the graphql subscription query we defined above to fetch the online user data.

Now, we will wrap the component with `Subscription` passing our graphql mutation constant that we imported. Replace the `return` with the following code:

```javascript
 return (
   <div className="onlineUsersWrapper">
+    <Subscription subscription={gql`
+      subscription getOnlineUsers {
+        online_users(order_by: {user: {name: asc }}) {
+          id
+          user {
+            name
+          }
+        }
+      }`}>
+      {({ loading, error, data }) => {
+        if (loading) {
+          return (<span>Loading...</span>);
+        }
+        if (error) {
+          console.error(error);
+          return (<span>Error!</span>);
+        }
+        if (data) {
+          const users = data.online_users;
+          const onlineUsersList = [];
+          users.forEach((u, index) => {
+            onlineUsersList.push(
+              <OnlineUser
+                key={index}
+                index={index}
+                user={u.user}
+              />);
+          });
+          return (
+            <Fragment>
+              <div className="sliderHeader">
+                Online users - {users.length}
+              </div>
+              {onlineUsersList}
+            </Fragment>
+          );
+        }
+      }}
+    </Subscription>
   </div>
 );
```

Now that we have the real data, let's remove the mock online user state

```javascript
class OnlineUsersWrapper extends Component {
  constructor(props) {
    super(props);
    this.client = props.client;

-   this.state = {
-     onlineUsers: [
-       { name: "someUser1" },
-       { name: "someUser2" }
-     ]
-   };
  }
  render() {
-   const onlineUsersList = [];
-   this.state.onlineUsers.forEach((user, index) => {
-     onlineUsersList.push(
-       <OnlineUser
-         key={index}
-         index={index}
-         user={user}
-       />);
-   });

    return (
      ...
    );
  }
}

```

How does this work?
-------------------

We are using the `<Subscription>` component which gives render props (similar to `<Query>` and `<Mutation>` components). The `data` prop gives the result of the realtime data for the query we have made.

Refresh your react app and see yourself online! Don't be surprised; There could be other users online as well.

Awesome! You have completed basic implementations of a GraphQL Query, Mutation and Subscriptions. Easy isn't it?
