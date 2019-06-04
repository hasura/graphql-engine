---
title: "Handle errors"
metaTitle: "Apollo Query Error Handling | GraphQL Android Apollo Tutorial"
metaDescription: "We will handle the GraphQL loading and error states in Android app using the Apollo onFailure and onResponse callbacks"
---

As we saw in the previous step, Apollo had `onFailure` and `onResponse` callback methods.

Now let's go back to the `getMyTodoQueryCloud` function that you wrote in the previous step.

```kotlin

  private fun getMyTodoQueryCloud() {
        // Init Query
        getMyTodosQuery = GetMyTodosQuery.builder().build()

        // Apollo runs query on background thread
        Network.apolloClient.query(getMyTodosQuery)?.enqueue(object : ApolloCall.Callback<GetMyTodosQuery.Data>() {
            override fun onFailure(error: ApolloException) {
                Log.d("Todo", error.toString() )
            }

            override fun onResponse(@NotNull response: Response<GetMyTodosQuery.Data>) {
                // Changing UI must be on UI thread
                Log.d("Todo", response.data().toString() )
                listItems = response.data()?.todos()?.toMutableList()
                activity?.runOnUiThread { updateTabs() }
            }
        })
    }

```

When this function is executed in the `onCreate`, we handle the completion of it from the `onError` or `onResponse` callback methods.

Now, the query could also end up in an `error` state due to various reasons. Sometimes the graphql query could be wrong, or the server isn't responding. Whatever may be the reason, the user facing UI should show something to convey that an error has occurred. Here we are just logging the error, but you can do retry or redirect logic here.
In this error state, typically you can send these error messages to third-party services to track what went wrong.

All said and done, these are two important states that need to be handled inside your fragment. What you have written above is basic, but sufficient for this tutorial.
