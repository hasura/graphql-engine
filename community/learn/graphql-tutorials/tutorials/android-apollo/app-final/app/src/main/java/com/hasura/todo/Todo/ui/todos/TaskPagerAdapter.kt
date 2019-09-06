package com.hasura.todo.Todo.ui.todos

import androidx.fragment.app.Fragment
import androidx.fragment.app.FragmentManager
import androidx.fragment.app.FragmentPagerAdapter
import androidx.viewpager.widget.PagerAdapter
import com.hasura.todo.Todo.ui.todos.TaskFragment.Companion.ACTIVE
import com.hasura.todo.Todo.ui.todos.TaskFragment.Companion.ALL
import com.hasura.todo.Todo.ui.todos.TaskFragment.Companion.COMPLETED

class TaskPagerAdapter(manager: FragmentManager) : FragmentPagerAdapter(manager) {

    /**
     * Return the Fragment associated with a specified position.
     */
    override fun getItem(position: Int): Fragment {
        return when(position){
            0 -> TaskFragment.newInstance(ALL)
            1 -> TaskFragment.newInstance(ACTIVE)
            2 -> TaskFragment.newInstance(COMPLETED)
            else -> Fragment()
        }
    }

    override fun getItemPosition(`object`: Any): Int {
        return PagerAdapter.POSITION_NONE
    }
    /**
     * Return the number of views available.
     */
    override fun getCount(): Int {
        return 3
    }

    override fun getPageTitle(position: Int): CharSequence? {
        return when(position){
            0 -> "ALL"
            1 -> "ACTIVE"
            2 -> "COMPLETED"
            else -> null
        }
    }
}