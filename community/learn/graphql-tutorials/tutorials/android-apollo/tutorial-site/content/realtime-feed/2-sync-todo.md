---
title: "Sync new todos"
metaTitle: "Sync new todos in public feed | GraphQL Android Apollo Tutorial"
metaDescription: "You will learn how to sync new todos added by other people in the public feed by fetching older and newer data using GraphQL Queries"
---

Once a new todo is entered in a public list, it needs to appear in the UI. Instead of automatically displaying the todo in the UI, we use a Feed like Notification banner which appears whenever a new todo is received.

In the Subscription method of the previous step, we only get the latest todo id not the complete todo. We will now write a simple query to fetch the list of public todos based on id.

Start off by writing a graphql query which takes in id as query variable.

```graphql
query getNewPublicTodos($latestVisibleId: Int) {
  todos(where: { is_public: { _eq: true }, id: { _gt: $latestVisibleId } }, order_by: { created_at: desc }) {
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

```kotlin
private fun getNewPublicTodosQuery(todoId: Int){
        //Init Query
        getNewPublicTodosQuery = GetNewPublicTodosQuery.builder().latestVisibleId(todoId -1).build()

        // Apollo runs query on background thread
        Network.apolloClient
            .query(getNewPublicTodosQuery)
            .responseFetcher(ApolloResponseFetchers.NETWORK_ONLY)
            .enqueue(object : ApolloCall.Callback<GetNewPublicTodosQuery.Data>() {
            override fun onFailure(error: ApolloException) {
                Log.d("Public Feed", error.toString() )
            }

            override fun onResponse(@NotNull response: Response<GetNewPublicTodosQuery.Data>) {
                // Changing UI must be on UI thread
                val publicTodoList = mutableListOf(response.data()!!).flatMap {
                        data -> data.todos().map{
                        data -> "@${data.user().name()} - ${data.title()}"
                    }
                }

                if(publicTodoList.isNotEmpty())
                firstVisibleId = mutableListOf(response.data()!!).flatMap {
                        data -> data.todos()
                }.last().id()

                listItems.addAll(0, publicTodoList.toMutableList())
                refreshListView()
            }
        })

    }

```

Let's update our action to click of `view` button in notification banner,

```kotlin
    // Show Notification View
    private fun showNotificationView(){
-        // TODO : More items to load
+        activity?.runOnUiThread {
+            if(notificationCount.size == 1)
+                listView.addHeaderView(notificationView)
+
+            setNotificationCountText()
+            listView.smoothScrollToPosition(0)
+        }
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

Update the `loadMoreItems` function to the following:

```kotlin

    private fun loadMoreItems(){
-        // TODO : More items to load
+        getOldPublicTodosQuery()
    }


  // Get Old Todos
+   private fun getOldPublicTodosQuery(){
+        //Init Query
+        getoldPublicTodosQuery = GetOldPublicTodosQuery.builder().oldestTodoId(lastVisibleId!!).build()
+
+        // Apollo runs query on background thread
+        Network.apolloClient
+            .query(getoldPublicTodosQuery)
+            .responseFetcher(ApolloResponseFetchers.NETWORK_ONLY)
+            .enqueue(object : ApolloCall.Callback<GetOldPublicTodosQuery.Data>() {
+                override fun onFailure(error: ApolloException) {
+                    Log.d("Public Feed", error.toString() )
+                }
+
+                override fun onResponse(@NotNull response: Response<GetOldPublicTodosQuery.Data>) {
+                    // Changing UI must be on UI thread
+                    val publicTodoList = mutableListOf(response.data()!!).flatMap {
+                            data -> data.todos().map{
+                            data -> "@${data.user().name()} - ${data.title()}"
+                        }
+                    }
+
+                    lastVisibleId = mutableListOf(response.data()!!).flatMap {
+                            data -> data.todos()
+                    }.last().id()
+
+                    listItems.addAll(publicTodoList.toMutableList())
+                    refreshListView()
+                    activity?.runOnUiThread {listView.smoothScrollToPosition(listItems.size)}
+                }
+            })
+
+    }
```

We are defining a query to fetch older public todos and making a `apolloClient.query` call to get the data from the cloud. Once we get the data, we update the `listItems` & reload the tableView to re-render the UI with the available list of public todos.

Try adding a new Public todo, it won't show up, as we are not doing anything on the Done Action of keyboard, lets add that.

```kotlin
    private fun addPublicTodo(title: String){
-        // TODO : Add public todo on press of done on keyboard
+        addNewPublicTodoMutationCloud(title)
    }
```

Now try adding a new todo to the public feed and you will see the notification appearing saying that a new task has arrived.
