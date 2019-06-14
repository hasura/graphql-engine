---
title: "Run mutation, update cache"
metaTitle: "Apollo Mutation Integration | GraphQL Android Apollo Tutorial"
metaDescription: "We will use the Apollo Mutation using apolloClient.mutate in Android app as an example to insert new data and update cache locally using apolloStore().read and apolloStore().write"
---

Now let's do the integration part. Add the following code below to remove the log and add the actual implementation:

```kotlin
    override fun onResponse(@NotNull response: Response<AddTodoMutation.Data>) {
                // get data from local cache and update the list
-                Log.d("Todo", response.data() )
+                val addedTodo = response.data()?.insert_todos()?.returning()?.get(0)
+
+                val todo = GetMyTodosQuery.Todo(
+                    addedTodo?.__typename()!!,
+                    addedTodo.id(),
+                    addedTodo.title(),
+                    addedTodo.created_at(),
+                    addedTodo.is_completed)
+
+
+                Network.apolloClient
+                    .apolloStore()
+                    .write(GetMyTodosQuery(), GetMyTodosQuery.Data(mutableListOf(todo))).execute()
+                getMyTodosQueryLocal()
            }
```

We are updating our local cache with this `apolloClient.apolloStore().write` function. You can also check the data in your local store which will be in the same shape as you have defined for your queries. This enables you to make fetch calls to the server or just the local cache as your need be. Plus, there are optimistic updates that you can do to your UI that we will learn later.

Let's also add the implementation of getting the todos from local cache, so we can update our views

```kotlin
// Fetch Todos from local cache
    fun getMyTodosQueryLocal(){

        getMyTodosQuery = GetMyTodosQuery.builder().build()
        Network.apolloClient
            .query(getMyTodosQuery)
            .responseFetcher(ApolloResponseFetchers.CACHE_FIRST)
            .enqueue(object : ApolloCall.Callback<GetMyTodosQuery.Data>() {
            override fun onFailure(e: ApolloException) {
                Log.d("Todo", e.toString())
            }

            override fun onResponse(response: Response<GetMyTodosQuery.Data>) {
                listItems = response.data()?.todos()?.toMutableList()
                activity?.runOnUiThread { updateTabs() }
            }
        })
    }
```

Now let's add this to the function invoked by done on the keyboard,

```kotlin
fun addTodo(title: String) {
-       // Todo : Add method to update todos
+        addTodoMutationCloud(title)
    }
```

We are invoking the `addTodoMutationCloud` function with text typed in the textfield.
This function's `title` argument is the mutation query's options, such as variables etc. We are now passing the variables required for the mutation.

Let's dissect what's happening in this code snippet.

Our goals were simple:

- Make a mutation to insert the new todo in the cloud.
- Once the mutation is done, we need to update the Cache & update the UI.

The `apolloClient.apolloStore().write` function is used to update the cache after a mutation occurs.
**IMPORTANT:** Caching is provided only for `query` operations. It isn't available for `mutation` operations.

### Direct cache access

You can directly read and update the cache as needed using `apolloStore()`. This functionality is useful when performing mutations or receiving subscription data, as you should always update the local cache to ensure consistency with the operation that was just performed. The ability to write to the cache directly also prevents you from needing to re-fetch data over the network after a mutation is performed.

## read

Unlike `apolloClient.query`, read will never make a request to your GraphQL server.

## write

We have already done the mutation to the graphql server using the mutate function. Our goal was to update the UI. This is where `apolloClient.apolloStore().write` comes to the rescue. `apolloClient.apolloStore().write` will allow you to change data in your local cache, but it is important to remember that they will not change any data on your server (exactly what we need).

Any watcher to the Apollo Client store will instantly see this update and render new UI accordingly.

We concatenate our new todo from our mutation with the list of existing todos and write the query back to the cache with `apolloClient.apolloStore().write`

Great! That was actually easy :)
