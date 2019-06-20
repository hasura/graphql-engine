package com.hasura.todo.Todo.ui.todos

import android.content.Context.INPUT_METHOD_SERVICE
import android.os.Bundle
import android.view.KeyEvent
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.view.inputmethod.EditorInfo
import android.view.inputmethod.InputMethodManager
import android.widget.EditText
import androidx.fragment.app.Fragment
import androidx.viewpager.widget.ViewPager
import com.google.android.material.tabs.TabLayout
import com.hasura.todo.Todo.R
import kotlinx.android.synthetic.main.fragment_todos.*
import kotlinx.android.synthetic.main.fragment_todos.view.*


class TodosFragment : Fragment(), TaskFragment.FragmentListener {

    override fun onCreateView(
        inflater: LayoutInflater,
        container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {

        val root = inflater.inflate(R.layout.fragment_todos, container, false)

        val input: EditText = root.title_text
        input.setOnEditorActionListener { v, actionId, event ->
            if ((event != null && (event.getKeyCode() == KeyEvent.KEYCODE_ENTER)) || (actionId == EditorInfo.IME_ACTION_DONE)) {

                // Add Todo
                if ( input.editableText.toString() != ""){
                    addTodo(input.editableText.toString())
                }

                // Dismiss Keyboard
                val imm = context?.getSystemService(INPUT_METHOD_SERVICE) as InputMethodManager
                imm.hideSoftInputFromWindow(view?.windowToken, 0)

                // Clear Input
                input.setText("")
            }
            false
        }

        val viewPager: ViewPager = root.viewPager
        val adapter = TaskPagerAdapter(childFragmentManager)
        viewPager.adapter = adapter
        viewPager.offscreenPageLimit = 5

        //TabLayout
        val tabLayout: TabLayout = root.tabLayout
        tabLayout.setupWithViewPager(viewPager)
        tabLayout.addOnTabSelectedListener(object : TabLayout.OnTabSelectedListener {
            override fun onTabSelected(tab: TabLayout.Tab?) {
                // Get current active fragment and update recycler view
                val frag  = childFragmentManager.findFragmentByTag("android:switcher:" + R.id.viewPager + ":" + tab?.position)
                if (frag != null) {
                    (frag as TaskFragment).refreshData()
                }
            }

            override fun onTabUnselected(tab: TabLayout.Tab?) {
            }

            override fun onTabReselected(tab: TabLayout.Tab?) {
            }
        })
        return root
    }

    private fun addTodo(title: String){
        val frag = childFragmentManager.findFragmentByTag("android:switcher:" + R.id.viewPager + ":" + viewPager.currentItem) as TaskFragment
        frag.addTodo(title)
    }

    override fun notifyDataSetChanged() {
        viewPager.adapter?.notifyDataSetChanged()
    }
}