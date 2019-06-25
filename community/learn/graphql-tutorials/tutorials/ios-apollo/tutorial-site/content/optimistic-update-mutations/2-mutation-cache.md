---
title: "Mutation and update cache"
metaTitle: "Apollo perform(mutation) for GraphQL mutation update | GraphQL iOS Apollo Tutorial"
metaDescription: "We will use the Apollo client perform(mutation) from apollo-ios as an example to modify existing data and update cache locally using readQuery and writeQuery and handle optimisticResponse"
---

import GithubLink from '../../src/GithubLink.js'

Now let's do the integration part. Open `Todo/TodoVC.swift` and add the following code at the bottom of the file:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/ios-apollo/app-final/Todo/TodoVC.swift" text="Todo/TodoVC.swift" />

```swift
    // Toggle Todos to Cloud
    private func toggleTodosMutationCloud(checked: Bool, index: Int!){
        apollo.perform(mutation: ToggleTodoMutation(id: filteredTodos[index].id, isCompleted: checked)) { (result, error) in
            guard let data = result?.data else { return }
            if data.updateTodos?.affectedRows == 1 { return } else {
                // With some error - revert to original state : Watcher will update the table anyways on local cache update
            self.toggleTodosMutationLocal(id: index, checked: checked)
            }
        }
    }
```

Lets also, add the function for adding the mutation to the local cache,

```swift
 // Toggle Todos local cache
    private func toggleTodosMutationLocal(id: Int, checked: Bool){
        _ = apollo.store.withinReadWriteTransaction{ transaction in
            let query = GetMyTodosQuery()
            try transaction.update(query: query) { (data: inout GetMyTodosQuery.Data) in
                let todos = data.todos
                guard let index = todos.firstIndex(where: {$0.id == id}) else {return}
                data.todos[index].isCompleted = checked
                
                _ = self.apollo.store.load(query: query).andThen({ (data) in
                    // Watch your data in local cache
                    // dump(data.data?.resultMap)
                    // Look for errors
                    // dump(data.errors)
                })
            }
        }
    }
    
```

We need to modify our `todoQueryCloud` to `todoQueryWacther` to watch for changes in a particular query on the local cache. To make sure, it is intialized properly and discarded we need to update our `viewWillAppear` and `viewWillDisapper`:

```swift
    // Todo Query Watcher from local cache
-    private func todoQueryCloud(){
-        apollo.fetch(query: GetMyTodosQuery()){ (result, error) in
+    private func todoQueryWatcher() -> GraphQLQueryWatcher<GetMyTodosQuery>{
+        return apollo.watch(query: GetMyTodosQuery(), resultHandler: {(result, error) in
            if ((error) != nil) {
                if SessionManager.shared.logout() {
                    self.performSegue(withIdentifier: "loginVC", sender: self)
                }
                return
            }
            guard let data = result?.data else { return }
            self.todos = data.todos
-           self.filteredTodos = data.todos
+           self.filteredTodos = self.getFilteredTodos(segmentIndex: self.todoFilter.selectedSegmentIndex)
                self.setupUI()
                self.todoTable.reloadData()
        })
    }
```

```swift
  override func viewWillAppear(_ animated: Bool) {
        if( SessionManager.shared.credentials?.idToken! != nil ) {
            apollo = NetworkManager.shared.apolloClient
            
            // Initialize a Watcher to check for updates
-            todoQueryCloud()
+            todoWatcher = todoQueryWatcher()
        }
    }
    
+    override func viewWillDisappear(_ animated: Bool) {
+            todoWatcher?.cancel()
+    }
```

`todoWatcher` will update the `todos` and `filteredTodos` when there is any update in the cache.

We already have the onChange handler `checkBox` for the tablecell. Let's update the function to make a mutation.

```swift
  // OnClick of Checkbox
    func checkBox(checked: Bool, index: Int!) {
        // Optimistic update & Change UI
        toggleTodosMutationLocal(id: filteredTodos[index].id, checked: checked)
        let index = self.todos.firstIndex{$0.id == filteredTodos[index].id}!
        todos[index].isCompleted = checked
        filteredTodos[index].isCompleted = checked
        todoTable.reloadRows(at: [IndexPath(row: index, section: 0)], with: UITableView.RowAnimation.automatic)
        
        // Update Network
        toggleTodosMutationCloud(checked: checked, index: index)
    }
```

The above code will just make a mutation, updating the todo's isCompleted property in the local cache. After updating the cache, we update the UI to make this change visible. And then call the `toggleTodosMutationCloud` to update the cloud. If there was any error, in updating the cloud, it will inturn update the local cache and through watcher, it will automatically revert to the original state.
