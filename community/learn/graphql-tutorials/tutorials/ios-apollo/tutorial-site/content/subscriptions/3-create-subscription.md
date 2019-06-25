---
title: "Create Subscription and Render Result"
metaTitle: "Apollo Subscription in iOS Swift | GraphQL iOS Apollo Tutorial"
metaDescription: "Integrate apollo-ios subscription method to watch for changes in realtime data. We use GraphQL subscriptions as an example to get live data in the iOS app"
---

import GithubLink from '../../src/GithubLink.js'

So let's define the graphql subscription to be used.

Open `Todo/OnlineVC.swift` and update following code,

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/ios-apollo/app-final/Todo/OnlineVC.swift" text="Todo/OnlineVC.swift" />

```swift
+    var onlineUsersSub: Cancellable?
// Subscribe for Online users
    private func subscribeOnlineUsers(){
+        onlineUsersSub = apollo.subscribe(subscription: GetOnlineUsersSubscription()) { (result, error) in
+            if((error) != nil) { dump(error); return }
+        }
        registerSelfAsOnlineTimer = Timer.scheduledTimer(timeInterval: 30, target: self, selector: #selector(updateLastSeenMutationCloud), userInfo: nil, repeats: true)
        registerSelfAsOnlineTimer?.fire()
    }
    
    
    // Un-subscribe for online users
    private func unSubscribeOnlineUsers(){
+        onlineUsersSub?.cancel()
        registerSelfAsOnlineTimer?.invalidate()
    }
    
```

We are creating a `Cancellable` varibale and the assing apollo.subscribe graphql subscription we defined above to fetch the online user data.

Now, we will update the UI with the results from this subscrition. Update the following in the `subscribeOnlineUsers` function:

```swift
 // Subscribe for Online users
    private func subscribeOnlineUsers(){
        onlineUsersSub = apollo.subscribe(subscription: GetOnlineUsersSubscription()) { (result, error) in
            if((error) != nil) { dump(error); return }
            guard let data = result?.data else { return }
+            self.onlineUsers = data.onlineUsers.compactMap{$0.user?.name}
+            DispatchQueue.main.async {
+                self.onlineUserCount.text = String(self.onlineUsers.count)
+                self.onlineUsersTable.reloadData()
+                self.setupUI()
+            }
        }
```

Now that we have the real data, let's remove the mock online users in `onlineUsers` variable

```swift
-   var onlineUsers: [String] = ["Loki", "Thor", "Dr Strange", "Hulk", "Mantis", "TChala", "Iron Man", "Thanos"]
+   var onlineUsers: [String] = []
```

How does this work?
-------------------

We are using the `Apollo.subscribe#subscription` which gives cancellable with GraphQLReseultSet (similar to `query` and `mutation`). The `result` object gives the result of the realtime data for the query we have made.

Re-run your ios app and see yourself online! Don't be surprised; There could be other users online as well.

Awesome! You have completed basic implementations of a GraphQL Query, Mutation and Subscriptions. Easy isn't it?
