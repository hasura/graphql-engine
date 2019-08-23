---
title: "Query Variables"
metaTitle: "Passing GraphQL Variables in Queries | GraphQL Android Apollo Tutorial"
metaDescription: "An Example of passing variables in GraphQL context and usage of Apollo GraphQL Mutation variables in Android app."
---

## What is a variable in GraphQL context?

GraphQL has a first-class way to factor dynamic values out of the query, and pass them as a separate dictionary. These values are called variables. In our case, we are defining the object to be inserted as a mutation.

So let's define the graphql mutation to be used. In our `TaskFragment.kt`, add below method for adding a new Todo

```kotlin
private fun addTodoMutationCloud(title: String) {
        // Init Query
        addTodoMutation = AddTodoMutation.builder().todo(title).isPublic(false).build()

        // Apollo runs query on background thread
        Network.apolloClient.mutate(addTodoMutation)?.enqueue(object : ApolloCall.Callback<AddTodoMutation.Data>() {
            override fun onFailure(error: ApolloException) {
                Log.d("Todo", error.toString() )
            }

            override fun onResponse(@NotNull response: Response<AddTodoMutation.Data>) {
                Log.d("Todo", response.data() )
            }
        })
    }
```

## What does this mutation do?

The mutation inserts into `todos` table with the `$title` variable being passed as one query variable. We are logging the result to see the reponse from the server.

Awesome! We have defined our first graphql mutation.
