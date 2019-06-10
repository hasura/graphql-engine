---
title: "Fetch public todos - subscription"
metaTitle: "Fetch public todos using Subscription | GraphQL Android Apollo Tutorial"
metaDescription: "You will learn how to make use of GraphQL Subscriptions to get notified whenever a new todo comes in Android app"
---

import GithubLink from '../../src/GithubLink.js'

Let's define the graphql query to be used:

Open `FeedFragment.kt` and add the following variable and initialize in `onCreate` and `onPause`.

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/android-apollo/app-final/app/src/main/java/com/hasura/todo/Todo/ui/feed/FeedFragment.kt" text="FeedFragment.kt" />

```kotin

    private lateinit var newPublicTodosSubscriptionQuery: NotifyNewPublicTodosSubscription
    private var newpublicTodoSubscription: ApolloSubscriptionCall<NotifyNewPublicTodosSubscription.Data>? = null

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        // Subscribe
        subscribeNewPublicTodo()
    }

    // Disable Subscriptions
    override fun onPause() {
        super.onPause()
        newpublicTodoSubscription?.cancel()
    }
```

Now let's define the subscription to get notified about new public todos

```kotlin
    private fun subscribeNewPublicTodo(){
        // Init Query
        newPublicTodosSubscriptionQuery = NotifyNewPublicTodosSubscription.builder().build()

        newpublicTodoSubscription = Network.apolloClient
            .subscribe(newPublicTodosSubscriptionQuery)

        newpublicTodoSubscription?.execute(object: ApolloSubscriptionCall
            .Callback<NotifyNewPublicTodosSubscription.Data> {
            override fun onFailure(e: ApolloException) {
                Log.d("Public Feed", e.toString())
            }

            override fun onResponse(response: Response<NotifyNewPublicTodosSubscription.Data>) {
                Log.d("Public Feed Subs", response.data().toString())

                val notifId: Int = mutableListOf(response.data()!!).flatMap {
                        data -> data.todos()
                }.first().id()

                if ( firstVisibleId != null && notifId != firstVisibleId)
                notificationCount.add(notifId)

                showNotificationView()
            }

            override fun onConnected() {
                Log.d("Public Feed", "Connected to WS" )
            }

            override fun onTerminated() {
                Log.d("Public Feeds", "Dis-connected from WS" )
            }

            override fun onCompleted() {
            }

        })
    }
```

Also lets add a function to fetch the initial public todos.

```kotlin

+    private lateinit var initialPublicTodosQuery: GetInitialPublicTodosQuery

    private fun getInitialPublicTodosQuery(){
        // Init Query
        initialPublicTodosQuery = GetInitialPublicTodosQuery.builder().build()

        // Apollo runs query on background thread
        Network.apolloClient
            .query(initialPublicTodosQuery)
            .responseFetcher(ApolloResponseFetchers.NETWORK_ONLY)
            .enqueue(object : ApolloCall.Callback<GetInitialPublicTodosQuery.Data>() {
            override fun onFailure(error: ApolloException) {
                Log.d("Public Feed", error.toString() )
            }

            override fun onResponse(@NotNull response: Response<GetInitialPublicTodosQuery.Data>) {
                // Changing UI must be on UI thread
                val publicTodoList = mutableListOf(response.data()!!).flatMap {
                        data -> data.todos().map{
                            data -> "@${data.user().name()} - ${data.title()}"
                        }
                }
                firstVisibleId = mutableListOf(response.data()!!).flatMap {
                        data -> data.todos()
                }.first().id()
                lastVisibleId = mutableListOf(response.data()!!).flatMap {
                        data -> data.todos()
                    }.last().id()

                listItems = publicTodoList.toMutableList()
                refreshListView()
                setupLoadMore()
            }
        })
    }

```

and add that in `onAttach`

```kotlin
    override fun onAttach(context: Context?) {
        super.onAttach(context)

        // Initial public todos
        getInitialPublicTodosQuery()
    }
```

## What does the Subscription do?

The query fetches `todos` with a simple condition; `is_public` must be true. We also limit the number of todos to 1, since we would just like to get notified whenever a new todo comes in.
We sort the todos by its latest createdAt time according to the schema. We specify which fields we need for the todos node.

Right now we don't return anything when new data comes in. `getInitialPublicTodosQuery` will map the result.data to the publicTodos so, we can view them on the list.
