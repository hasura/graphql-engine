package com.hasura.todo.Todo.ui.feed

import android.content.Context
import android.os.Bundle
import android.util.Log
import android.view.KeyEvent
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.view.inputmethod.EditorInfo
import android.view.inputmethod.InputMethodManager
import android.widget.*
import androidx.fragment.app.Fragment
import com.apollographql.apollo.ApolloCall
import com.apollographql.apollo.ApolloSubscriptionCall
import com.apollographql.apollo.api.Response
import com.apollographql.apollo.exception.ApolloException
import com.apollographql.apollo.fetcher.ApolloResponseFetchers
import com.hasura.todo.*
import com.hasura.todo.Todo.R
import com.hasura.todo.Todo.network.Network
import kotlinx.android.synthetic.main.feed_notifiction.view.*
import kotlinx.android.synthetic.main.fragment_feed.view.*
import kotlinx.android.synthetic.main.load_more.view.*
import org.jetbrains.annotations.NotNull

class FeedFragment : Fragment() {
    private lateinit var listView: ListView
    private lateinit var notificationCountText: TextView
    private var adapter: ArrayAdapter<String>? = null

    private val notificationCount: MutableList<Int> = mutableListOf()
    private var firstVisibleId: Int? = null
    private var lastVisibleId: Int? = null

    private lateinit var addTodoMutation: AddTodoMutation
    private lateinit var initialPublicTodosQuery: GetInitialPublicTodosQuery
    private lateinit var getNewPublicTodosQuery: GetNewPublicTodosQuery
    private lateinit var getoldPublicTodosQuery: GetOldPublicTodosQuery
    private lateinit var newPublicTodosSubscriptionQuery: NotifyNewPublicTodosSubscription
    private var newpublicTodoSubscription: ApolloSubscriptionCall<NotifyNewPublicTodosSubscription.Data>? = null

    private var notificationView: View? = null
    private var footer: View? = null

    companion object{
        @JvmStatic
        private var listItems: MutableList<String> = mutableListOf()
    }

    override fun onAttach(context: Context?) {
        super.onAttach(context)

        // Initial public todos
        getInitialPublicTodosQuery()
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        // Subscribe
        subscribeNewPublicTodo()
    }

    override fun onCreateView(
        inflater: LayoutInflater,
        container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {
        val root = inflater.inflate(R.layout.fragment_feed, container, false)
        val input = root.title_text
        input.setOnEditorActionListener { v, actionId, event ->
            if ((event != null && (event.keyCode == KeyEvent.KEYCODE_ENTER)) || (actionId == EditorInfo.IME_ACTION_DONE)) {
                // Add Todo
                if ( input.editableText.toString() != ""){
                    addPublicTodo(input.editableText.toString())
                }

                // Dismiss Keyboard
                val imm = context?.getSystemService(Context.INPUT_METHOD_SERVICE) as InputMethodManager
                imm.hideSoftInputFromWindow(view?.windowToken, 0)

                // Clear Input
                input.setText("")
            }
            false
        }

        listView = root.list_feed

        footer = (activity?.getSystemService(Context.LAYOUT_INFLATER_SERVICE) as LayoutInflater).inflate(
            R.layout.load_more,
            listView,
            false
        )
        notificationView = (activity?.getSystemService(Context.LAYOUT_INFLATER_SERVICE) as LayoutInflater).inflate(
            R.layout.feed_notifiction,
            listView,
            false
        )
        val notificationButton = notificationView?.viewNotification
        notificationCountText = notificationView?.textNotification!!
        setNotificationCountText()

        notificationButton?.setOnClickListener{ viewNotificationFeed() }
        val loadMore: Button = footer?.loadMore!!
        loadMore.setOnClickListener{ loadMoreItems() }


        adapter = ArrayAdapter(requireContext(), android.R.layout.simple_list_item_1, listItems)
        listView.adapter = adapter

        return root
    }

    private fun addPublicTodo(title: String){
        addNewPublicTodoMutationCloud(title)
    }

    private fun loadMoreItems(){
        getOldPublicTodosQuery()
    }

    private fun viewNotificationFeed(){
        notificationCount.forEach{ getNewPublicTodosQuery(it) }

        listView.removeHeaderView(notificationView)
        notificationCount.clear()
    }

    private fun setNotificationCountText(){
        notificationCountText.text = "${notificationCount.size} New tasks available"
    }

    // Refresh data
    private fun refreshListView() {
        activity?.runOnUiThread {

            // update data in our adapter
            adapter?.clear()
            adapter?.addAll(listItems)

            listView.invalidateViews()
            adapter?.notifyDataSetChanged()
        }
    }

    // Show Notification View
    private fun showNotificationView(){
        activity?.runOnUiThread {
            if(notificationCount.size == 1)
                listView.addHeaderView(notificationView)

            setNotificationCountText()
            listView.smoothScrollToPosition(0)
        }

    }

    // Setup Notification
    private fun setupLoadMore(){
        activity?.runOnUiThread {
            if(listItems.isNotEmpty())
                listView.addFooterView(footer)
        }
    }



    // Queries and mutations
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

    private fun getOldPublicTodosQuery(){
        //Init Query
        getoldPublicTodosQuery = GetOldPublicTodosQuery.builder().oldestTodoId(lastVisibleId!!).build()

        // Apollo runs query on background thread
        Network.apolloClient
            .query(getoldPublicTodosQuery)
            .responseFetcher(ApolloResponseFetchers.NETWORK_ONLY)
            .enqueue(object : ApolloCall.Callback<GetOldPublicTodosQuery.Data>() {
                override fun onFailure(error: ApolloException) {
                    Log.d("Public Feed", error.toString() )
                }

                override fun onResponse(@NotNull response: Response<GetOldPublicTodosQuery.Data>) {
                    // Changing UI must be on UI thread
                    val publicTodoList = mutableListOf(response.data()!!).flatMap {
                            data -> data.todos().map{
                            data -> "@${data.user().name()} - ${data.title()}"
                        }
                    }

                    lastVisibleId = mutableListOf(response.data()!!).flatMap {
                            data -> data.todos()
                    }.last().id()

                    listItems.addAll(publicTodoList.toMutableList())
                    refreshListView()
                    activity?.runOnUiThread {listView.smoothScrollToPosition(listItems.size)}
                }
            })

    }

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

    private fun addNewPublicTodoMutationCloud(title: String){
        // Init Query
        addTodoMutation = AddTodoMutation.builder().todo(title).isPublic(true).build()

        // Apollo runs query on background thread
        Network.apolloClient.mutate(addTodoMutation)?.enqueue(object : ApolloCall.Callback<AddTodoMutation.Data>() {
            override fun onFailure(error: ApolloException) {
                Log.d("Public Feed", error.toString() )
            }

            override fun onResponse(@NotNull response: Response<AddTodoMutation.Data>) {
                // get data from local cache and update the list
                Log.d("Public feed", "Successfully Added Public Todo")
                Network.apolloClient.enableSubscriptions()
            }
        })

    }

    // Disable Subscriptions
    override fun onPause() {
        super.onPause()
        newpublicTodoSubscription?.cancel()
    }
}