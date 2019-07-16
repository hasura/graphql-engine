---
title: "Create Subscription and Render Result"
metaTitle: "Apollo Subscription Component | GraphQL React Native Apollo Tutorial"
metaDescription: "Integrate React Apollo Subscription Component to watch for changes in realtime data. We use GraphQL subscriptions as an example to get live data in the React Native app"
---

Open `src/data/online_fetch.dart` and add the following code.
```dart
+static String fetchUsers = """
+   subscription fetchOnlineUsers {
+   online_users {
+     user {
+       name
+     }
+   }
+ }
""";
```

Open `screens/abs/dashboard/online.dart`
Now,Wrap the ListView with `Subscription` widget passing graphql subscription string. 

```dart
+   Subscription(
+         "fetchOnlineUsers",
+         OnlineFetch.fetchUsers,
+         builder: ({
+           bool loading,
+           dynamic payload,
+           dynamic error,
+         }) {
+           if (payload != null) {
-            Expanded(
+             return Expanded(
+               child: ListView.builder(
-                 itemCount: onlineList.list.length,
+                 itemCount: payload['online_users'].length,
                  itemBuilder: (context, index) {
                    return Card(
                      child: ListTile(
                       title: Text(
-                           title: Text(onlineList.list[index]),
+                           payload['online_users'][index]['user']['name']),
                      ),
                    );
                  },
                ),
              );
+            } else {
+             return Text("Fetching Online Users");
+           }
+         },
+       ),
```

How does this work?
-------------------
We are using the `Subscription` widget which gives payload (similar data in to `Query` and `Mutation` widget) of the realtime data for the query we have made.

Refresh your app and see yourself online! Don't be surprised; There could be other users online as well.

Awesome! You have completed basic implementations of a GraphQL Query, Mutation and Subscriptions. Easy isn't it?
