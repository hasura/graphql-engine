---
title: "Create Subscription and Render Result"
---

import GithubLink from "../../src/GithubLink.js";

So let's define the graphql subscription to be used.

Open `src/screens/UsersScreen.js` and add the following code, below the other imports

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/react-native-apollo/app-final/src/screens/UserScreen.js" text="UserScreen.js" />

```javascript
import {Subscription} from 'react-apollo';
```

Import `gql` from `graphql-tag` and define the subscription query:

```js
+ import gql from 'graphql-tag';


+ const SUBSCRIBE_TO_ONLINE_USERS = gql`
+   subscription {
+     online_users(order_by: {user: {name: asc}}) {
+       user {
+         name
+         id
+       }
+       id
+     }
+   }
+`; 
```

We are importing the `Subscription` component from `react-apollo` and the graphql subscription query we defined above to fetch the online user data.

Now, we will wrap the component with `Subscription` passing our graphql subscription constant that we imported. Replace the `return` with the following code:

```javascript
-   const data = {
-      "online_users": [
-        {
-          user: {
-            name: "User 1",
-            id: 1
-          },
-          id: 1
-        },
-        {
-          user: {
-            name: "User   2",
-            id: 2
-          },
-          id: 2
-        },
-      ]
-   }
+  return (
+    <View style={styles.container}>
+      <Subscription
+        subscription={subscribeToOnlineUsers}
+      >
+        {
+          ({data, loading, error}) => {
+            if (loading) { return <CenterSpinner />}
+            if (error) {
+              return <Text> Error </Text>
+            }
+            return (
+              <ScrollView style={styles.scrollView} contentContainerStyle={styles.scrollViewContainer}>
+              <FlatList
+                data={data.online_users}
+                renderItem={({item}) => <UserItem item={item} />}
+                keyExtractor={(item) => item.user.name}
+              />
+              </ScrollView>
+            )
+          }
+        }
+      </Subscription>
+    </View>
+  );
```

How does this work?
-------------------
We are using the `<Subscription>` component which gives render props (similar to `<Query>` and `<Mutation>` components). The `data` prop gives the result of the realtime data for the query we have made.

Refresh your app and see yourself online! Don't be surprised; There could be other users online as well.

Awesome! You have completed basic implementations of a GraphQL Query, Mutation and Subscriptions. Easy isn't it?
