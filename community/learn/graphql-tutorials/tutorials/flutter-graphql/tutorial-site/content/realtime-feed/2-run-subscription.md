---
title: "Detect new todos - Integration"
metaTitle: "Fetch public todos using Subscription | GraphQL Flutter Tutorial"
metaDescription: "You will learn how to make use of GraphQL Subscriptions to get notified whenever a new todo comes in Flutter app."
---

Go to `lib/data/` and create a file named `feed_fetch.dart` inside that create a class named `FeedFetch` add the following subscription string.

```dart
static String fetchNewNotification = """subscription fetchNewNotification {
  todos(where: {is_public: {_eq: true}}, limit: 1, order_by: {created_at: desc}) {
    id
  }
}
""";
```

Now wrap custom button i.e `New Notification` in subscription widget. But before that, add following variables in your `_FeedsState` class.

```dart
+  static int _lastLatestFeedId;
+  static int _oldestFeedId;
+  static int _newTodoCount = 0;
+  static int _previousId = 0;
+  static int _newId = 0;
+  GraphQLClient _client;
```

And initialize \_client in initState method.

```dart
+ @override
+  void initState() {
+    WidgetsBinding.instance.addPostFrameCallback((_) {
+      _client = GraphQLProvider.of(context).value;
+    });
+    super.initState();
+  }
```

Now let's wrap the subscription

```dart
+ Subscription(
+          "fetchNewNotification",
+          FeedFetch.fetchNewNotification,
+          builder: ({
+           bool loading,
+           dynamic payload,
+           dynamic error,
+         }) {
+           if (payload != null) {
+             _newId = payload['todos'][0]['id'];
+              if (_previousId != 0) {
+               _newTodoCount = _newTodoCount + (_newId - _previousId);
+             }
+             _previousId = payload['todos'][0]['id'];
+             if (_newTodoCount != 0) {
-               CustomButton(
+               return CustomButton(
                 onTap: () {
                   print("loading");
                 },
                 height: 50,
                 text: " $_newTodoCount New Notification",
                 width: MediaQuery.of(context).size.width / 2,
               );
-              } else
-               return SizedBox();
-           } else {
-             return SizedBox();
-           }
-         },
-       )
```

The `fetchNewNotification` does the following:

1. Starts a subscription to the last todo in the database.
2. Whenever data is received, it looks at the todos and compare previous id with current id via subscription, if new id is greater then previous id, it will increase the count of notification.

Awesome! You are now detecting new todo updates from the backend.

Now let's make this button functional and add feeds to our list.
