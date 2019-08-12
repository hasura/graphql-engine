---
title: "Sync new todos"
metaTitle: "Sync new todos in public feed | GraphQL iOS Apollo Tutorial"
metaDescription: "You will learn how to sync new todos added by other people in the public feed by fetching older and newer data using GraphQL Queries"
---

Once a new todo is entered in a public list, it needs to appear in the UI. Instead of automatically displaying the todo in the UI, we use a Feed like Notification banner which appears whenever a new todo is received.

In the Subscription component of the previous step, we only get the latest todo id not the complete todo. We will now write a simple query to fetch the list of public todos based on id.

Start off by writing a graphql query which takes in id as query variable.

```graphql
query getNewPublicTodos ($latestVisibleId: Int) {
    todos(where: { is_public: { _eq: true}, id: {_gt: $latestVisibleId}}, order_by: { created_at: desc }) {
        id
        title
        created_at
        user {
            name
        }
    }
}
```

Now that we have this query, we can write our functions to fetch public todos based on id's.

```javascript
// Get Subscribed Todos from notification
    private func getNotifTodos(id: Int){
        apollo.fetch(query: GetNewPublicTodosQuery(latestVisibleId: id)) { (result, err) in
            guard let data = result?.data else { return }
            // Show notification
            let todos = data.todos.compactMap{ PublicTodo(title: $0.title, username: $0.user.name, id: $0.id)}
            self.addPublicTodoLocal(todos: todos)
        }
    }
```

Let's update our action to notification with below,

```swift
    // On click of view button in Notification
    @IBAction func viewNotificationAction(_ sender: Any) {
+        self.getNotifTodos(id: publicTodosNotif[0])
+        self.publicTodosNotif.removeAll()
+        toggleNotifView()
    }
```

To get the older todos on click of load more button, lets add the graphql query for it,

```swift
query getOldPublicTodos ($oldestTodoId: Int!) {
    todos (where: { is_public: { _eq: true}, id: {_lt: $oldestTodoId}}, limit: 7, order_by: { created_at: desc }) {
        id
        title
        created_at
        user {
            name
        }
    }
}
```

Update the `loadMoreAction` function to the following & update it's IBAction:

```swift
   
    // On click of load more button
    @IBAction func loadMoreAction(_ sender: Any) {
+        self.setupUI(loading: true)
+        getOldPublicTodos(id: self.publicTodos.last!.id)
    }
    

  // Get Old Todos
+  query getOldPublicTodos ($oldestTodoId: Int!) {
+    todos (where: { is_public: { _eq: true}, id: {_lt: $oldestTodoId}}, limit: 7, order_by: { created_at: desc }) {
+        id
+        title
+        created_at
+        user {
+            name
+        }
+    }
+  }
```

We are defining a query to fetch older public todos and making a `apollo.fetch#query` call to get the data from the cloud. Once we get the data, we update the `publicTodos` & reload the tableView to re-render the UI with the available list of public todos.

Try adding a new Public todo, it won't show up, as we are not doing anything on the Done Action of keyboard, lets add that.

```swift
func textFieldShouldReturn(_ textField: UITextField) -> Bool{
        // Add todos on return & clear text field & remvove keyboard
        if let title = textField.text, title != empty {
+            self.addTodoToPublicCloud(title: title)
            textField.text = empty
            textField.resignFirstResponder()
        }
        return true
    }
```

Now try adding a new todo to the public feed and you will see the notification appearing saying that a new task has arrived.
