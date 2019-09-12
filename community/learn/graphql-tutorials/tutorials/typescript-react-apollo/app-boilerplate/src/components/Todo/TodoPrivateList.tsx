import React, { Fragment, useState } from "react";
import TodoItem from "./TodoItem";
import TodoFilters from "./TodoFilters";

type Todo = {
  id: number,
  title: string,
  is_completed: boolean
};

const TodoPrivateList = () => {

  const [filter, setFilter] = useState<string>("all");

  const todos = [
    {
      id: 1,
      title: "This is private todo 1",
      is_completed: true
    },
    {
      id: 2,
      title: "This is private todo 2",
      is_completed: false
    }
  ];

  const filterResults = (filter: string): void => {
    setFilter(filter);
  };

  const clearCompleted = () => {
  };

  let filteredTodos = todos;
  if (filter === "active") {
    filteredTodos = todos.filter((todo: Todo) => todo.is_completed !== true);
  } else if (filter === "completed") {
    filteredTodos = todos.filter((todo: Todo) => todo.is_completed === true);
  }

  const todoList = filteredTodos.map((todo: Todo, index: number) => (
    <TodoItem
      key={'item'+index}
      index={index}
      todo={todo}
    />
  ));

  return (
    <Fragment>
      <div className="todoListWrapper">
        <ul>
          { todoList }
        </ul>
      </div>

      <TodoFilters
        todos={filteredTodos}
        currentFilter={filter}
        filterResultsFn={filterResults}
        clearCompletedFn={clearCompleted}
      />
    </Fragment>
  );
}

export default TodoPrivateList;
