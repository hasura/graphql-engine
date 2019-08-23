package com.hasura.todo.Todo.ui.todos

import android.content.Context
import android.os.Bundle
import android.util.Log
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.Button
import androidx.fragment.app.Fragment
import androidx.recyclerview.widget.DividerItemDecoration
import androidx.recyclerview.widget.LinearLayoutManager
import com.apollographql.apollo.ApolloCall
import com.apollographql.apollo.api.Response
import com.apollographql.apollo.exception.ApolloException
import com.apollographql.apollo.fetcher.ApolloResponseFetchers
import com.hasura.todo.*
import com.hasura.todo.Todo.R
import com.hasura.todo.Todo.network.Network
import kotlinx.android.synthetic.main.task_todos.*
import kotlinx.android.synthetic.main.task_todos.view.*
import org.jetbrains.annotations.NotNull
import java.util.*

private const val COMPLETE_STATUS = "status"

class TaskFragment : Fragment(), TaskAdapter.TaskItemClickListener {

    private var completeStatus: String? = null
    private lateinit var getMyTodosQuery: GetMyTodosQuery
    private lateinit var toggleTodoMutation: ToggleTodoMutation
    private lateinit var addTodoMutation: AddTodoMutation
    private lateinit var removeTodoMutation: RemoveTodoMutation
    private lateinit var clearCompletedMutation: ClearCompletedMutation


    interface FragmentListener {
        fun notifyDataSetChanged()
    }
    private var filteredListItems = listItems

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        arguments?.let {
            completeStatus = it.getString(COMPLETE_STATUS)
        }

        // Get Initial Todos from cloud
        getMyTodoQueryCloud()
    }

    override fun onCreateView(
        inflater: LayoutInflater, container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {
        // Inflate the layout for this fragment
        var root = inflater.inflate(R.layout.task_todos, container, false)
        var removeAllCompleted: Button = root.removeAllCompleted
        removeAllCompleted.setOnClickListener {
            removeAllCompleted()
        }
        return root
    }

    override fun onViewCreated(view: View, savedInstanceState: Bundle?) {
        super.onViewCreated(view, savedInstanceState)
        taskRecyclerView.layoutManager = LinearLayoutManager(activity)
        taskRecyclerView.addItemDecoration(DividerItemDecoration(taskRecyclerView.context, DividerItemDecoration.VERTICAL))
    }

    fun refreshData() {
        updateTabs()
    }

    fun updateTabs() {
        filteredListItems = listItems
        when (completeStatus) {
            ALL -> getFilteredData(filteredListItems)
            ACTIVE -> getFilteredData(filteredListItems?.filter { todo -> !todo.is_completed } as MutableList<GetMyTodosQuery.Todo>)
            COMPLETED -> getFilteredData(filteredListItems?.filter { task -> task.is_completed } as MutableList<GetMyTodosQuery.Todo>)
        }
        taskRecyclerView.adapter?.notifyDataSetChanged()
    }

    private fun getFilteredData(list: MutableList<GetMyTodosQuery.Todo>?) {
        if (list != null && list.isNotEmpty()) {
            emptyMessageTextView.visibility = View.INVISIBLE
            taskRecyclerView.visibility = View.VISIBLE
            val taskAdapter = TaskAdapter(list, this@TaskFragment)
            taskRecyclerView.swapAdapter(taskAdapter, true)
            if (completeStatus == COMPLETED) {
                removeAllCompleted.visibility = View.VISIBLE
            }
        } else {
            removeAllCompleted.visibility = View.INVISIBLE
            emptyMessageTextView.visibility = View.VISIBLE
            when (completeStatus) {
                ACTIVE -> emptyMessageTextView.text = "No Active Tasks!"
                COMPLETED -> emptyMessageTextView.text = "No Completed Tasks Yet!"
            }
            activity?.runOnUiThread {taskRecyclerView.visibility = View.INVISIBLE}
        }
    }

    override fun updateTaskCompleteStatus(taskId: Int, completeFlag: Boolean) {
        toggleTodoMutationCloud(taskId, completeFlag)
    }

    fun addTodo(title: String) {
        addTodoMutationCloud(title)
    }

    override fun delete(taskId: Int) {
        removeTodoMutationCloud(taskId)
    }

    private fun removeAllCompleted() {
        removeAllCompletedCloud()
    }


    companion object {
        const val ALL = "ALL"
        const val ACTIVE = "ACTIVE"
        const val COMPLETED = "COMPLETED"
        private var fragmentListener: FragmentListener? = null

        var listItems: MutableList<GetMyTodosQuery.Todo>? = null

        @JvmStatic
        fun newInstance(completeStatus: String): TaskFragment {
            return TaskFragment().apply {
                arguments = Bundle().apply {
                    putString(COMPLETE_STATUS, completeStatus)
                }
            }
        }
    }


    // Queries & Mutations

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

    private fun addTodoMutationCloud(title: String) {
        // Init Query
        addTodoMutation = AddTodoMutation.builder().todo(title).isPublic(false).build()

        // Apollo runs query on background thread
        Network.apolloClient.mutate(addTodoMutation)?.enqueue(object : ApolloCall.Callback<AddTodoMutation.Data>() {
            override fun onFailure(error: ApolloException) {
                Log.d("Todo", error.toString() )
            }

            override fun onResponse(@NotNull response: Response<AddTodoMutation.Data>) {
                // get data from local cache and update the list
                val addedTodo = response.data()?.insert_todos()?.returning()?.get(0)

                val todo = GetMyTodosQuery.Todo(
                    addedTodo?.__typename()!!,
                    addedTodo.id(),
                    addedTodo.title(),
                    addedTodo.created_at(),
                    addedTodo.is_completed)


                Network.apolloClient
                    .apolloStore()
                    .write(GetMyTodosQuery(), GetMyTodosQuery.Data(mutableListOf(todo))).execute()
                getMyTodosQueryLocal()
            }
        })
    }

    private fun removeTodoMutationCloud(todoId: Int){
        // Init Query
        removeTodoMutation = RemoveTodoMutation.builder().id(todoId).build()

        // Apollo runs query on background thread
        Network.apolloClient.mutate(removeTodoMutation)?.enqueue(object : ApolloCall.Callback<RemoveTodoMutation.Data>() {
            override fun onFailure(error: ApolloException) {
                Log.d("Todo", error.toString() )
            }

            override fun onResponse(@NotNull response: Response<RemoveTodoMutation.Data>) {
                // get data from local cache and update the list
                val index = listItems?.indexOfFirst { todo ->  todo.id() == todoId}
                var todos = (listItems?.toMutableList())?.removeAt(index!!)

                Network.apolloClient
                    .apolloStore()
                    .write(GetMyTodosQuery(), GetMyTodosQuery.Data(mutableListOf(todos))).execute()
                getMyTodosQueryLocal()
            }
        })
    }

    private fun removeAllCompletedCloud(){
        // Init Query
        clearCompletedMutation = ClearCompletedMutation.builder().build()

        // Apollo runs query on background thread
        Network.apolloClient.mutate(clearCompletedMutation)?.enqueue(object : ApolloCall.Callback<ClearCompletedMutation.Data>() {
            override fun onFailure(error: ApolloException) {
                Log.d("Todo", error.toString() )
            }

            override fun onResponse(@NotNull response: Response<ClearCompletedMutation.Data>) {
                // get data from local cache and update the list
                val todos = listItems?.filter { task -> task.is_completed }
                Network.apolloClient
                    .apolloStore()
                    .write(GetMyTodosQuery(), GetMyTodosQuery.Data(todos!!)).execute()
                getMyTodosQueryLocal()
            }
        })
    }

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

    override fun onAttach(context: Context?) {
        super.onAttach(context)

        // check if parent Fragment implements listener
        if (parentFragment is FragmentListener) {
            fragmentListener = parentFragment as FragmentListener
        } else {
            throw RuntimeException("$parentFragment must implement FragmentListener")
        }

    }

    override fun onDetach() {
        super.onDetach()
        fragmentListener = null
    }
}