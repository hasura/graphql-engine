---
title: "Create Subscription and Render Result"
metaTitle: "Apollo Subscription in Android | GraphQL Android Apollo Tutorial"
metaDescription: "Integrate Android Apollo Subscription to watch for changes in realtime data. We use GraphQL subscriptions as an example to get live data in the Android app"
---

import GithubLink from '../../src/GithubLink.js'

So let's define the graphql subscription to be used.

Open `OnlineFragment.kt` and update following code,

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/android-apollo/app-final/app/src/main/java/com/hasura/todo/Todo/ui/online/OnlineFragment.kt" text="OnlineFragment.kt" />

```kotlin
+    private lateinit var onlineUsersSubscriptionQuery: GetOnlineUsersSubscription
    private lateinit var lastSeenMutation: UpdateLastSeenMutation
+    private var onlineUsersSubscription: ApolloSubscriptionCall<GetOnlineUsersSubscription.Data>? = null

+    // Queries and mutations
+    private fun subscribeOnlineUsers() {
+        // Init Query
+        onlineUsersSubscriptionQuery = GetOnlineUsersSubscription.builder().build()
+
+        onlineUsersSubscription = Network.apolloClient.subscribe(onlineUsersSubscriptionQuery)
+
+        onlineUsersSubscription?.execute(object: ApolloSubscriptionCall.Callback<GetOnlineUsersSubscription.Data> {
+            override fun onFailure(e: ApolloException) {
+                Log.d("Online Users", e.localizedMessage)
+            }
+
+            override fun onResponse(response: Response<GetOnlineUsersSubscription.Data>) {
+                Log.d("Subs", response.data().toString())
+
+                val userList = mutableListOf(response.data()!!).flatMap {
+                        data -> data.online_users().map{
+                    data -> data.user()?.name()
+                    }
+                }
+
+                listItems = userList.toMutableList()
+
+                refreshListView(listItems)
+            }
+
+            override fun onConnected() {
+                Log.d("Online Users", "Connected to WS" )
+            }
+
+            override fun onTerminated() {
+                Log.d("Online Users", "Dis-connected from WS" )
+            }
+
+            override fun onCompleted() {
+            }
+
+        })
+    }
```

Now, we will update the UI with the results from this subscrition. Update the `onCreate` with the `subscribeOnlineUsers` function invocation:

```kotlin
     override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        // Subscribe to online users list
        subscribeOnlineUsers()
    }
```

We need to discard the web socket connection as soon as the fragment is not in view,

```kotlin
   // Disable Subscriptions
    override fun onPause() {
        super.onPause()
        onlineUsersSubscription?.cancel()
        updateLastSeenTimer.cancel()
    }
```

Now that we have the real data, let's remove the mock online users in `listItems` variable

```swift
private val listItems = arrayOf(
-        "SomeUser1",
-        "SomeUser2 ",
-        "SomeUser3",
-        "SomeUser4",
-        "SomeUser5",
-        "SomeUser6"
    )
```

## How does this work?

We are using the `apolloClient.subscribe` which gives callback with onFailure, onResponse, onConnected, OnDisconnected and onCompleted methods to override. The `response.data()` object gives the result of the realtime data for the query we have made.

Re-run your android app and see yourself online! Don't be surprised; There could be other users online as well.

Awesome! You have completed basic implementations of a GraphQL Query, Mutation and Subscriptions. Easy isn't it?
