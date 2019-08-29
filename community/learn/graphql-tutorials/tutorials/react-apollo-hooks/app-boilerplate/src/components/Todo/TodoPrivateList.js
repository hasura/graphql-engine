import React, { useState, Fragment } from "react";

import TodoItem from "./TodoItem";
import TodoFilters from "./TodoFilters";

const TodoPrivateList = props => {
  const [state, setState] = useState({
    filter: "all",
    clearInProgress: false,
    todos: [
      {
        id: "1",
        title: "This is private todo 1",
        is_completed: true,
        is_public: false
      },
      {
        id: "2",
        title: "This is private todo 2",
        is_completed: false,
        is_public: false
      }
    ]
  });

  const filterResults = filter => {
    setState({
      ...state,
      filter: filter
    });
  };

  const clearCompleted = () => {};

  let filteredTodos = state.todos;
  if (state.filter === "active") {
    filteredTodos = state.todos.filter(todo => todo.is_completed !== true);
  } else if (state.filter === "completed") {
    filteredTodos = state.todos.filter(todo => todo.is_completed === true);
  }

  const todoList = [];
  filteredTodos.forEach((todo, index) => {
    todoList.push(<TodoItem key={index} index={index} todo={todo} />);
  });

  return (
    <Fragment>
      <div className="todoListWrapper">
        <ul>{todoList}</ul>
      </div>

      <TodoFilters
        todos={filteredTodos}
        currentFilter={state.filter}
        filterResultsFn={filterResults}
        clearCompletedFn={clearCompleted}
        clearInProgress={state.clearInProgress}
      />
    </Fragment>
  );
};

export default TodoPrivateList;
