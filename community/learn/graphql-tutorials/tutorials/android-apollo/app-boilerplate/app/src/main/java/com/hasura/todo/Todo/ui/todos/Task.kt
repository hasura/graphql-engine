package com.hasura.todo.Todo.ui.todos

class Task(
    private val id: Int = -1,
    private val name: String,
    private var completeFlag: Boolean = false) {

    fun getId() = id
    fun getName() = name
    fun getCompleteStatus() = completeFlag


    fun setCompleteStatus(completeFlag: Boolean) {
        this.completeFlag = completeFlag
    }
}
