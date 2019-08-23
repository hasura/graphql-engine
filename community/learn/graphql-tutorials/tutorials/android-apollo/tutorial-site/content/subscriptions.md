---
title: "Subscriptions to show online users"
metaTitle: "Update last seen of user with Mutation | GraphQL Android Apollo Tutorial"
metaDescription: "GraphQL Mutation to update last seen of user to make them available online. Use setInterval to trigger mutation every few seconds "
---

import GithubLink from '../src/GithubLink.js'

We cruised through our GraphQL queries and mutations. We queried for todos, added a new todo, updated an existing todo, removed an existing todo.

Now let's get to the exciting part.

## GraphQL Subscriptions

We have a section of UI which displays the list of online users. So far we have made queries to fetch data and display them on the UI. But typically online users data is dynamic.

We can make use of GraphQL Subscription API to get realtime data from the graphql server to efficiently handle this.

But but but...

We need to tell the server that the user who is logged in is online. We have to poll our server to do a mutation which updates the `lastSeen` timestamp value of the user.

We have to make this change to see yourself online first. Remember that you are already logged in, registered your data in the server, but not updated your `lastSeen` value.?

The goal is to update every few seconds from the client that you are online. Ideally you should do this after you have successfully authenticated with Auth0. So let's update some code to handle this.

Open `OnlineFragment.kt` and add the following functions and define a variable `updateLastSeenTimer` of type `Timer`.

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/android-apollo/app-final/app/src/main/java/com/hasura/todo/Todo/ui/online/OnlineFragment.kt" text="OnlineFragment.kt" />

```kotlin
+   private lateinit var updateLastSeenTimer: Timer
+   private lateinit var lastSeenMutation: UpdateLastSeenMutation

    override fun onCreateView(
        inflater: LayoutInflater,
        container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {
        val root = inflater.inflate(R.layout.fragment_online, container, false)

        onlineUserCount = root.text_online
        setOnlineUsersCount()

        listView = root.findViewById(R.id.list_online)
        adapter = ArrayAdapter(requireContext(), android.R.layout.simple_list_item_1, listItems)
        listView.adapter = adapter


+        updateLastSeenTimer = fixedRateTimer("timer",false,0,25000){
+            activity?.runOnUiThread {
+                updateLastSeen()
+            }
+        }

        return root
    }
```

Lets also add the `updateLastSeen` we defined to run after every interval of 25secs.

```kotlin
private fun updateLastSeen(){
        val ISO8601 = SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSSSSZ")

        val cal: Calendar = Calendar.getInstance()
        val date : Date = cal.time
        val formattedDate : String = ISO8601.format(date)

        // Init Query
        lastSeenMutation = UpdateLastSeenMutation.builder().now(formattedDate).build()

        Network.apolloClient.mutate(lastSeenMutation).enqueue(object: ApolloCall.Callback<UpdateLastSeenMutation.Data>(){
            override fun onFailure(e: ApolloException) {
                Log.d("Online Users", e.toString())
            }

            override fun onResponse(response: Response<UpdateLastSeenMutation.Data>) {
                Log.d("Online Users", "Successfully Updated Last Seen :  $response")
            }
        })
    }
```

Again, we are making use of `apolloClient.mutate` to update the `users` table of the database.

Great! Now the metadata about whether the user is online will be available in the backend. Let's now do the integration to display realtime data of online users.
