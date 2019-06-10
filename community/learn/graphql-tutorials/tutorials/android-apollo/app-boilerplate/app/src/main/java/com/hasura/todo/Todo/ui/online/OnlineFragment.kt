package com.hasura.todo.Todo.ui.online

import android.content.Intent
import android.os.Bundle
import android.view.*
import android.widget.ArrayAdapter
import android.widget.ListView
import android.widget.TextView
import androidx.fragment.app.Fragment
import com.hasura.todo.Todo.Login
import com.hasura.todo.Todo.R
import kotlinx.android.synthetic.main.fragment_online.view.*

class OnlineFragment : Fragment() {

    private lateinit var listView: ListView
    private lateinit var onlineUserCount: TextView

    private val listItems = arrayOf(
        "SomeUser1",
        "SomeUser2 ",
        "SomeUser3",
        "SomeUser4",
        "SomeUser5",
        "SomeUser6"
    )

    override fun onCreateView(
        inflater: LayoutInflater,
        container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {
        val root = inflater.inflate(R.layout.fragment_online, container, false)
        onlineUserCount = root.text_online
        setOnlineUsersCount()

        listView = root.findViewById(R.id.list_online)

        val adapter = ArrayAdapter<String>(activity, android.R.layout.simple_list_item_1, listItems)
        listView.adapter = adapter

        return root
    }

    override fun onActivityCreated(savedInstanceState: Bundle?) {
        super.onActivityCreated(savedInstanceState)
        setHasOptionsMenu(true)
    }

    override fun onCreateOptionsMenu(menu: Menu, inflater: MenuInflater) {
        super.onCreateOptionsMenu(menu, inflater)
        inflater.inflate(R.menu.logout, menu)
    }

    override fun onOptionsItemSelected(item: MenuItem): Boolean {
        // handle item selection
        when (item.itemId) {
            R.id.logout ->
                logout()
            else -> return super.onOptionsItemSelected(item)
        }
        return true
    }

    private fun setOnlineUsersCount(){
        onlineUserCount.text = "Total Online Users: ${listItems.size}"
    }

    private fun logout() {
        val intent = Intent(requireContext(), Login::class.java)
        intent.putExtra(Login.KEY_CLEAR_CREDENTIALS, true)
        startActivity(intent)
        activity?.finish()
    }
}