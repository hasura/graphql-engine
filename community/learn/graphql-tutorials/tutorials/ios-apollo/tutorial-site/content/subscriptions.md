---
title: "Subscriptions to show online users"
metaTitle: "Update last seen of user with Mutation | GraphQL iOS Apollo Tutorial"
metaDescription: "GraphQL Mutation to update last seen of user to make them available online. Use setInterval to trigger mutation every few seconds "
---

import GithubLink from '../src/GithubLink.js'

We cruised through our GraphQL queries and mutations. We queried for todos, added a new todo, updated an existing todo, removed an existing todo.

Now let's get to the exciting part.

GraphQL Subscriptions
---------------------

We have a section of UI which displays the list of online users. So far we have made queries to fetch data and display them on the UI. But typically online users data is dynamic.

We can make use of GraphQL Subscription API to get realtime data from the graphql server to efficiently handle this.

But but but...

We need to tell the server that the user who is logged in is online. We have to poll our server to do a mutation which updates the `lastSeen` timestamp value of the user.

We have to make this change to see yourself online first. Remember that you are already logged in, registered your data in the server, but not updated your `lastSeen` value.?

The goal is to update every few seconds from the client that you are online. Ideally you should do this after you have successfully authenticated with Auth0. So let's update some code to handle this. 

Open `Todo/OnlineVC.swift` and add the following functions and define a variable `registerSelfAsOnlineTimer` of type `Timer`.

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/ios-apollo/app-final/Todo/OnlineVC.swift" text="Todo/OnlineVC.swift" />

```swift
+    var apollo: ApolloClient!
+    var registerSelfAsOnlineTimer: Timer?

    // Subscribe for Online users
+    private func subscribeOnlineUsers(){
+        registerSelfAsOnlineTimer = Timer.scheduledTimer(timeInterval: 30, target: self, selector: #selector+(updateLastSeenMutationCloud), userInfo: nil, repeats: true)
+        registerSelfAsOnlineTimer?.fire()
+    }
    
    // Un-subscribe for online users
+    private func unSubscribeOnlineUsers(){
+        registerSelfAsOnlineTimer?.invalidate()
+    }
```

Lets also, provide the invocation of these methods in `viewWillAppear` and `viewWillDisappear`

```swift
override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        if(SessionManager.shared.credentials?.accessToken! != nil) {
            self.apollo = NetworkManager.shared.apolloClient
        }
+        subscribeOnlineUsers()
    }
    
    override func viewWillDisappear(_ animated: Bool) {
        super.viewWillDisappear(animated)
+        unSubscribeOnlineUsers()
    }
```

Now let's write the definition of the `updateLastSeenMutationCloud`.

```swift
// Update Last Seen Mutation cloud
    @objc func updateLastSeenMutationCloud() {
        let date = Date()
        let formatter = DateFormatter()
        formatter.calendar = Calendar(identifier: .iso8601)
        formatter.locale = Locale(identifier: "en_US_POSIX")
        formatter.timeZone = TimeZone(secondsFromGMT: 0)
        formatter.dateFormat = "yyyy-MM-dd'T'HH:mm:ss.SSSXXXXX"
        print(formatter.string(from: date))
        let currentTime = String(formatter.string(from: date))
        apollo.perform(mutation: UpdateLastSeenMutation(now: currentTime)) { (result, error) in
            if((error) != nil) { dump(error); return }
            guard let data = result?.data else { return }
            dump(data)
        }
    }
```

Again, we are making use of `apollo.perform#mutation` to update the `users` table of the database.

Great! Now the metadata about whether the user is online will be available in the backend. Let's now do the integration to display realtime data of online users.
