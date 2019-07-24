---
title: "Sync new todos"
metaTitle: "Sync new todos in public feed | GraphQL Flutter Tutorial"
metaDescription: "You will learn how to sync new todos added by other people in the public feed by fetching older and newer data using GraphQL Queries"
---


In the previous section, we made a button that shows up only when there are new public todos in the database. Now lets make this button functional i.e. on pressing this button, newer todos should be fetched from the backend, synced with the local todos and the button must be dismissed.

Go to `lib/data/feed_fetch.dart`, and add following query strings in it.

```dart
+  static String loadMoreTodos = """ query loadMoreTodos (\$oldestTodoId: Int!) {
+       todos (where: { is_public: { _eq: true}, id: {_lt: \$oldestTodoId}}, limit: 7, order_by: { 
+             created_at: desc }) {
+         id
+         title
+         created_at
+         user {
+           name
+         }
+       }
+     }""";
+ static String newTodos = """query newTodos (\$latestVisibleId: Int!) {
+      todos(where: { is_public: { _eq: true}, id: {_gt: \$latestVisibleId}}, order_by: { created_at: +      desc }) {
+        id
+        title
+        created_at
+        user {
+          name
+        }
+      }
+    }""";
```

Now, whenever we navigate to feeds tab, we want current todos and on pressing new notification, we want to add new todos in existing list and also on tap of load more, we want to fetch older todos.

Let's update our code accordingly step by step

First let's fetch current todos and new notification todos.

```dart
        Subscription(
          "fetchNewNotification",
          FeedFetch.fetchNewNotification,
          builder: ({
            bool loading,
            dynamic payload,
            dynamic error,
          }) {
            if (payload != null) {
              _newId = payload['todos'][0]['id'];

              if (_previousId != 0) {
                _newTodoCount = _newTodoCount + (_newId - _previousId);
              } 
+              else {
+                _lastLatestFeedId = _newId;
+                _client
+                    .query(
+                  QueryOptions(
+                    document: FeedFetch.loadMoreTodos,
+                    variables: {"oldestTodoId": _newId + 1},
+                  ),
+               )
+                   .then((onValue) {
+                 for (var todo in onValue.data.data['todos']) {
+                   feedList.addFeed(todo['id'].toString(), todo['title'],
+                       todo['user']['name']);
+                 }
+                 setState(() {});
+                }).catchError((onError) {
+                  print(onError);
+                });
+              }
              _previousId = payload['todos'][0]['id'];

              if (_newTodoCount != 0) {
                return CustomButton(
                  onTap: () {
+                    _client
+                       .query(
+                     QueryOptions(
+                       document: FeedFetch.newTodos,
+                       variables: {"latestVisibleId": _lastLatestFeedId},
+                     ),
+                   )
+                       .then((onValue) {
+                     for (var todo in onValue.data.data['todos'].reversed) {
+                       feedList.addfirstFeed(todo['id'].toString(),
+                           todo['title'], todo['user']['name']);
+                     }
+                     _lastLatestFeedId = int.parse(feedList.list.first.id);
+                     _newTodoCount = 0;
+                     setState(() {});
+                   }).catchError((onError) {
+                     print(onError);
+                   });
                 },
                  height: 50,
                  text: " $_newTodoCount New Notification",
                  width: MediaQuery.of(context).size.width / 2,
                );
              } else
                return SizedBox();
            } else {
              return SizedBox();
            }
          },
        )
         Expanded(
          child: ListView.builder(
            itemCount: feedList.list.length,
            itemBuilder: (context, index) {
              return FeedTile(
-                 feedList.list[feedList.list.length - index - 1].username,
-                 feed: feedList.list[feedList.list.length - index - 1].feed);
+                 username: feedList.list[index].username,
+                 feed: feedList.list[index].feed);
            },
          ),
        ),
```

So the above code is fetching some already existing todo from database and adding them to `feedList` to the very first time and then on tap of new notification, it is updating `feedList` with new todos.

Now let's add functionality to our load more button by again using `_client.query`.

```dart
       CustomButton(
          onTap: () {
-            print("load more");
+           _oldestFeedId = int.parse(feedList.list.last.id);
+           _client
+               .query(
+             QueryOptions(
+               document: FeedFetch.loadMoreTodos,
+               variables: {"oldestTodoId": _oldestFeedId},
+             ),
+           )
+               .then((onValue) {
+             for (var todo in onValue.data.data['todos']) {
+               feedList.addFeed(
+                   todo['id'].toString(), todo['title'], todo['user']['name']);
+             }
+             setState(() {});
+           }).catchError((onError) {
+             print(onError);
+           });
          },
          height: 50,
          text: "Load More",
          width: MediaQuery.of(context).size.width / 3,
        )
```
And yes to add new public todo, you can follow same as adding private todo.

With this, your fully functional realtime todo app is ready.
