package com.hasura.todo.Todo.ui.feed

import android.content.Context
import android.os.Bundle
import android.view.KeyEvent
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.view.inputmethod.EditorInfo
import android.view.inputmethod.InputMethodManager
import android.widget.ArrayAdapter
import android.widget.Button
import android.widget.ListView
import android.widget.TextView
import androidx.fragment.app.Fragment
import com.hasura.todo.Todo.R
import kotlinx.android.synthetic.main.feed_notifiction.view.*
import kotlinx.android.synthetic.main.fragment_feed.view.*
import kotlinx.android.synthetic.main.load_more.view.*

class FeedFragment : Fragment() {
    private lateinit var listView: ListView
    private lateinit var notificationCountText: TextView

    private val listItems = arrayOf(
        "@SomeUser1 - This is public todo 1",
        "@SomeUser2 - This is public todo 2",
        "@SomeUser3 - This is public todo 3",
        "@SomeUser4 - This is public todo 4",
        "@SomeUser5 - This is public todo 5",
        "@SomeUser6 - This is public todo 6"
    )
    private val notificationCount: MutableList<Int> = mutableListOf()

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
        val footer = inflater.inflate(R.layout.load_more, container, false)
        val notificationView = inflater.inflate(R.layout.feed_notifiction, container, false)
        val notificationButton = notificationView.viewNotification
        notificationCountText = notificationView.textNotification
        setNotificationCountText()

        notificationButton.setOnClickListener{ v -> viewNotificationFeed() }
        val loadMore: Button = footer.loadMore
        loadMore.setOnClickListener{ v -> loadMoreItems() }

        listView = root.list_feed
        listView.addHeaderView(notificationView)
        listView.addFooterView(footer)


        val adapter = ArrayAdapter<String>(activity, android.R.layout.simple_list_item_1, listItems)
        listView.adapter = adapter

        return root
    }

    private fun addPublicTodo(title: String){
        // TODO : Add public todo on press of done on keyboard
    }

    private fun loadMoreItems(){
        // TODO : More items to load
    }

    private fun viewNotificationFeed(){
        // TODO : More items to load
    }

    private fun setNotificationCountText(){
        notificationCountText.text = "${notificationCount.size} New tasks available"
    }
}