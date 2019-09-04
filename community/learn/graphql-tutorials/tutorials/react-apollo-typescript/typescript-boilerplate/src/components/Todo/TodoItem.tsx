import * as React from 'react';

import { Todo } from './TodoTypeDefs';

interface TodoItemType {
  index: number,
  todo: Todo
}

const TodoItem = ({index, todo}: TodoItemType) => {

  const removeTodo = (e: any) => {
    e.preventDefault();
    e.stopPropagation();
  };

  const toggleTodo = () => {};

  return (
    <li key={index}>
      <div className="view">
        <div className="round">
          <input
            checked={todo.is_completed}
            type="checkbox"
            id={todo.id}
            onChange={toggleTodo}
          />
          <label htmlFor={todo.id}/>
        </div>
      </div>

      <div className={"labelContent" + (todo.is_completed ? " completed" : '')}>
        <div>
          {todo.title}
        </div>
      </div>

      <button className="closeBtn" onClick={removeTodo}>
        x
      </button>
    </li>
  );
};

export default TodoItem;
