---
title: "Run mutation, update cache"
metaTitle: "Apollo Mutation using apollo.perform | GraphQL iOS Apollo Tutorial"
metaDescription: "We will use the Apollo Client apollo.perform in iOS app as an example to insert new data and update cache locally using read and write methods."
---

Now let's do the integration part. Open `Todo/TodoVC.swift` and add the following code below the previous one:

```swift
// Add Todo to Cloud
    private func addTodoMutationCloud(title: String){
        apollo.perform(mutation: AddTodoMutation(todo: title, isPublic: false)) { (result, error) in
            guard let data = result?.data else { return }
            let newTodo = data.insertTodos!.returning.compactMap({GetMyTodosQuery.Data.Todo(id: $0.id, title: $0.title, createdAt: $0.createdAt, isCompleted: $0.isCompleted)})
            self.todos.insert(contentsOf: newTodo, at: 0)
            self.filteredTodos.insert(contentsOf: newTodo, at: 0)
            
+            // Update local cache
+            self.addTodoMutationLocal(todo: newTodo[0])
            
            // Update view
            DispatchQueue.main.async {
                let indexPaths = (0 ..< newTodo.count).map { IndexPath(row: $0, section: 0) }
                self.todoTable.beginUpdates()
                self.todoTable.insertRows(at: indexPaths , with: .automatic)
                self.todoTable.endUpdates()
            }
            
        }
    }
    
+    // Add Todo to local cache
+    func addTodoMutationLocal(todo: GetMyTodosQuery.Data.Todo) {
+        _ = apollo.store.withinReadWriteTransaction{ transaction in
+            let query = GetMyTodosQuery()
+            try transaction.update(query: query) { (data: inout GetMyTodosQuery.Data) in
+                data.todos.insert(todo, at: 0)
+                
+                _ = self.apollo.store.load(query: query).andThen({ (data) in
+                    // Watch your data in local cache
+                    // dump(data.data?.resultMap)
+                    // Look for errors
+                    //dump(data.errors)
+                })
+                
+            }
+        }
+    }
```

We are upadting our local cache with this `addTodoMutationLocal` function. You can also check the data in your local store which will be in the same shape as you have defined for your queries. This enables you to make fetch calls to the server or just the local cache as your need be. Plus, there are optimistic updates that you can do to your UI that we will learn later.

Now let's handle the Keyboard Done press for this mutation.

```swift
      func textFieldShouldReturn(_ textField: UITextField) -> Bool{
        // Add todos on return & clear text field & remvove keyboard
        if let title = textField.text, title != empty {
+            addTodoMutationCloud(title: title)
            textField.text = empty
            textField.resignFirstResponder()
        }
        return true
    }
```

We are invoking the `addTodoMutationCloud` function with text typed in the textfield.
This function's `title` argument is the mutation query's options, such as variables etc. We are now passing the variables required for the mutation.

Let's dissect what's happening in this code snippet.

Our goals were simple:

- Make a mutation to insert the new todo in the cloud.
- Once the mutation is done, we need to update the UI & update the Cache.

The update function is used to update the cache after a mutation occurs.

### Direct cache access

You can directly read and update the cache as needed using Swift's `inout` parameters. This functionality is useful when performing mutations or receiving subscription data, as you should always update the local cache to ensure consistency with the operation that was just performed. The ability to write to the cache directly also prevents you from needing to re-fetch data over the network after a mutation is performed.

read
----

Unlike `apollo.fetch#query`, read will never make a request to your GraphQL server. It will always read from the cache. So we make a read request to the cache to get the current list of todos.

update
------

We have already done the mutation to the graphql server using the mutate function. Our goal was to update the UI. This is where `update` comes to the rescue. `update` will allow you to change data in your local cache, but it is important to remember that they will not change any data on your server (exactly what we need).

  Any watcher to the Apollo Client store will instantly see this update and render new UI accordingly.

We concatenate our new todo from our mutation with the list of existing todos and write the query back to the cache with `update`

Great! That was actually easy :)

