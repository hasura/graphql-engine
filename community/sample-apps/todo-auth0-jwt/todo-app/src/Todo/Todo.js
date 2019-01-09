import React from 'react';
import { Mutation } from "react-apollo";
import './Todo.css';

import {
  QUERY_TODO,
  MUTATION_TODO_UPDATE,
  MUTATION_TODO_DELETE
} from './graphQueries/todoQueries';

const handleTodoToggle = (toggleTodo, todo) => {
  toggleTodo({
    variables: {
      todoId: todo.id,
      set: {
        completed: !todo.completed
      }
    },
    update: (cache, { data: { update_todo }}) => {
      const data = cache.readQuery({ query: QUERY_TODO })
      const toggledTodo = data.todo.find(t => t.id === todo.id)
      toggledTodo.completed = !todo.completed;
      cache.writeQuery({
        query: QUERY_TODO,
        data
      })
    }
  })
}

const handleTodoDelete = (deleteTodo, todo) => {
  deleteTodo({
    variables: {
      todoId: todo.id
    },
    update: (cache, { data: { update_todo }}) => {
      const data = cache.readQuery({ query: QUERY_TODO })
      data.todo = data.todo.filter(t => {
        return t.id !== todo.id
      })
      cache.writeQuery({
        query: QUERY_TODO,
        data
      })
    }
  })
}

const Todo = ({ todo }) => (
  <Mutation mutation={MUTATION_TODO_UPDATE}>
    {(updateTodo) => {
      return (
        <div className="parentContainer">
          <li className="todoItem"
            onClick={e => {
              handleTodoToggle(updateTodo, todo)
            }}>
            {
              todo.completed ?
                <strike className="todoLabel">{todo.task}</strike> :
                <label className="todoLabel">{todo.task}</label>
            }
            <Mutation mutation={MUTATION_TODO_DELETE}>
              {(deleteTodo) => {
                return (
                  <label className="deleteLabel"
                    onClick={e => {
                      e.preventDefault();
                      e.stopPropagation();
                      handleTodoDelete(deleteTodo, todo)
                    }}>
                    Delete
                  </label>
                )
              }}
            </Mutation>
          </li>
        </div>
      )
    }}
  </Mutation>
)

export default Todo;
