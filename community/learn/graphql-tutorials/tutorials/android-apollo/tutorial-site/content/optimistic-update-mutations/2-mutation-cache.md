---
title: "Mutation and update cache"
metaTitle: "apolloClient.mutate for GraphQL mutation update | GraphQL Android Apollo Tutorial"
metaDescription: "We will use the Apollo client.mutate from android apollo as an example to modify existing data and update cache locally using read and write methods of apolloStore and handle optimisticResponse"
---

import GithubLink from '../../src/GithubLink.js'

Now let's do the integration part. Open `TaskFragment.kt` and add the following code at the bottom of the file:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/android-apollo/app-final/app/src/main/java/com/hasura/todo/Todo/ui/todos/TaskFragment.kt" text="TaskFragment.kt" />

```kotlin
    private fun toggleTodoMutationCloud(todoId: Int, completeFlag: Boolean){
        // Init Query
        toggleTodoMutation = ToggleTodoMutation.builder().id(todoId).isCompleted(completeFlag).build()
        getMyTodosQuery = GetMyTodosQuery.builder().build()

        val index = listItems?.indexOfFirst { todo ->  todo.id() == todoId}
        var todos = listItems?.toMutableList()?.get(index!!)


        val todo = GetMyTodosQuery.Todo(
            todos?.__typename()!!,
            todos.id(),
            todos.title(),
            todos.created_at(),
            completeFlag)

        var updatedList = listItems?.toMutableList()
        updatedList?.set(index!!, todo)



        // Optimistic Update
        Network.apolloClient
            .apolloStore()
            .writeOptimisticUpdatesAndPublish(GetMyTodosQuery(), GetMyTodosQuery.Data(mutableListOf(todo)), UUID.randomUUID()).execute()
        getMyTodosQueryLocal()


        // Apollo runs query on background thread
        Network.apolloClient.mutate(toggleTodoMutation)?.enqueue(object : ApolloCall.Callback<ToggleTodoMutation.Data>() {
            override fun onFailure(error: ApolloException) {
                Log.d("Todo", error.toString() )
            }

            override fun onResponse(@NotNull response: Response<ToggleTodoMutation.Data>) {
                Network.apolloClient.apolloStore().write(toggleTodoMutation, response.data()!!)
                getMyTodosQueryLocal()
            }
        })
    }
```

We are making an optimistic update to the the `apolloClient.apolloStore()` and then doing the actual mutation over the cloud.
Lets add this in our `updateTaskCompleteStatus` action handler,

```kotlin
    override fun updateTaskCompleteStatus(taskId: Int, completeFlag: Boolean) {
-        // Todo : Method for updating the complete status for the task
+        toggleTodoMutationCloud(taskId, completeFlag)
    }
```

We already have the `getMyTodosQueryLocal` which queries the data from local cache and udpates the list.
