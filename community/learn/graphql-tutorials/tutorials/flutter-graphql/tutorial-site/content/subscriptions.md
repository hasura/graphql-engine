---
title: "Subscriptions to show online users"
metaTitle: "Update last seen of user with Mutation | GraphQL Flutter Tutorial"
metaDescription: "GraphQL Mutation to update last seen of user to make them available online. Use setInterval to trigger mutation every few seconds "
---

import GithubLink from "../src/GithubLink.js";

We cruised through our GraphQL queries and mutations. We queried for todos, added a new todo item, updated an existing todo, removed an existing todo item.

Now let's get to the exciting part.

GraphQL Subscriptions
---------------------

We have a section of UI which displays the list of online users. So far we have made queries to fetch data and display them on the UI. But typically online users data is dynamic.

We can make use of GraphQL Subscription API to get realtime data from the GraphQL server to efficiently handle this.

But but but...

We need to tell the server that the user who is logged in is online. We have to poll our server to do a mutation which updates the `last_seen` timestamp value of the user.

We have to make this change to see ourself online first. Remember that we are already logged in and registered your data in the server, but not updated your `last_seen` value?

The goal is to update it every few seconds from the client that you are online. Ideally you should do this after you have successfully authenticated. So let's do it on the entrypoint of the app i.e. `lib/screens/tabs/todos/all.dart`. We instantiate `GraphQLClient`. 
First, lets define the mutation that sets `last_seen` to the current timestamp. For that create a file named `online_fetch.dart` inside `lib/data` and add the code below.

```dart
+ static String updateStatus =
+      """ mutation updateLastSeen (\$now: timestamptz!) {
+         update_users(where: {}, _set: {last_seen: \$now}) {
+         affected_rows
+       }
+     }""";
```


Now we shall create a function `runOnlineMutation` to start polling at regular interval of 30 sec in our `All` class (or widget) itself and call that function in init method of the class. 



```dart
+  static GraphQLClient _client;
+  runOnlineMutation(context) {
+    _client = GraphQLProvider.of(context).value;
+    Future.doWhile(
+      () async {
+        _client.mutate(
+          MutationOptions(
+            document: OnlineFetch.updateStatus,
+            variables: {
+              'now': DateTime.now().toUtc().toIso8601String(),
+            },
+          ),
+        );
+        await Future.delayed(Duration(seconds: 30));
+        return true;
+      },
+    );
+  }
  @override
    void initState() {
+     WidgetsBinding.instance
+       .addPostFrameCallback((_) => runOnlineMutation(context));
      super.initState();
    }
```

Again, we are making use of `_client.mutate` to update the `users` table of the database.

Great! Now the metadata about whether the user is online will be available in the backend. Let's now do the integration to display realtime data of online users.
