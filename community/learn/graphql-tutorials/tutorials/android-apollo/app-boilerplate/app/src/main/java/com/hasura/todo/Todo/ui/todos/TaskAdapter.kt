package com.hasura.todo.Todo.ui.todos

import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.CheckBox
import android.widget.ImageView
import android.widget.TextView
import androidx.recyclerview.widget.RecyclerView
import com.hasura.todo.Todo.R
import kotlinx.android.synthetic.main.task_item.view.*

class TaskAdapter(
    private val taskList: MutableList<Task>,
    private val taskItemClickListener: TaskItemClickListener
) : RecyclerView.Adapter<TaskAdapter.TaskViewHolder>() {


    interface TaskItemClickListener {
        fun delete(taskId: Int)
        fun updateTaskCompleteStatus(taskId: Int, completFlag: Boolean)
    }

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): TaskViewHolder {
        val view = LayoutInflater.from(parent.context).inflate(R.layout.task_item, parent, false)
        return TaskViewHolder(view)
    }

    override fun getItemCount(): Int {
        return taskList.size
    }

    override fun onBindViewHolder(holder: TaskViewHolder, position: Int) {
        holder.taskName.text = taskList[position].getName()
        holder.taskCompleteStatus.isChecked = taskList[position].getCompleteStatus()

        holder.taskDelete.setOnClickListener {
            taskItemClickListener.delete(taskList[position].getId())
        }
        holder.taskCompleteStatus.setOnClickListener {

            taskItemClickListener.updateTaskCompleteStatus(
                taskList[position].getId(),
                holder.taskCompleteStatus.isChecked
            )
        }
    }

    class TaskViewHolder(view: View) : RecyclerView.ViewHolder(view) {
        val taskName: TextView = view.taskName
        val taskDelete: ImageView = view.deleteTask
        val taskCompleteStatus: CheckBox = view.completeCheck
    }
}