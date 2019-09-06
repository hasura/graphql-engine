---
title: "Fetch public todos - subscription"
metaTitle: "Fetch public todos using Subscription | GraphQL iOS Apollo Tutorial"
metaDescription: "You will learn how to make use of GraphQL Subscriptions to get notified whenever a new todo comes in iOS app"
---

import GithubLink from '../../src/GithubLink.js'

Let's define the graphql query to be used:

Open `Todo/FeedVC.swift` and add the following variable and initialize in `viewWillAppear` and `viewWillDisappear`.

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/ios-apollo/app-final/Todo/FeedVC.swift" text="Todo/FeedVC.swift" />

```swift
+    var apollo: ApolloClient!
+    var newPublicTodos: Cancellable?

    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        if( SessionManager.shared.credentials?.accessToken! != nil ) {
            self.apollo = NetworkManager.shared.apolloClient
        }
        Utils.shared.setLeftPaddingInput(ofInput: todoInput)
-        toggleNotif()
+        subscribeNewPublicTodos()
        setupUI(loading: false)
    }
    
    override func viewWillDisappear(_ animated: Bool) {
        super.viewWillDisappear(animated)
+        unSubscribeNewPublicTodos()
    }
```

Now let's define the subscription to get notified about new public todos

```swift
// Subscribe to New Public Todos
    private func subscribeNewPublicTodos() {
        newPublicTodos = apollo.subscribe(subscription: NotifyNewPublicTodosSubscription()) { (result, error) in
            if((error) != nil) { dump(error); return }
            guard let data = result?.data else { return }
            
        }
    }
    
```

Also lets add a function to fetch the initial public todos.

```swift
    // Get Public Todos from cloud
    private func getInitialPublicTodos(){
        apollo.fetch(query: GetInitialPublicTodosQuery()) { (result, err) in
            guard let data = result?.data else { return }
            let newTodos = data.todos.compactMap { PublicTodo(title: $0.title, username: $0.user.name, id: $0.id) }
            self.publicTodos = newTodos
            DispatchQueue.main.async {
                self.todoPublicTable.reloadData()
                self.setupUI(loading: false)
            }
        }
    }
```

and add that in `viewWillAppear`
```swift
super.viewWillAppear(animated)
        if( SessionManager.shared.credentials?.accessToken! != nil ) {
            self.apollo = NetworkManager.shared.apolloClient
+            if self.publicTodos.isEmpty {
+                getInitialPublicTodos()
+            }
        }
```

What does the Subscription do?
-----------------------------

The query fetches `todos` with a simple condition; `is_public` must be true. We also limit the number of todos to 1, since we would just like to get notified whenever a new todo comes in.
We sort the todos by its latest createdAt time according to the schema. We specify which fields we need for the todos node.

Right now we don't return anything when new data comes in. `getInitialPublicTodos` will map the result.data to the publicTodos so, we can view them on the tableView.
