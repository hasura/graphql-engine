/* eslint-disable jsx-a11y/anchor-is-valid */
import * as React from 'react';
import { TodoItem } from './TodoItem';

interface filterResults {
  (filter: string): void
}

interface TodoFiltersArgs {
  todos: TodoItem[],
  currentFilter: string,
  filterResultsFn: filterResults,
  clearCompletedFn: VoidFunction
}

const TodoFilters = ({
  todos,
  currentFilter,
  filterResultsFn,
  clearCompletedFn
}: TodoFiltersArgs) => {
  
  const filterResultsHandler:filterResults = (filter: string) => { // enum type def
    filterResultsFn(filter);
  };

  // The clear completed button if these are personal todos
  const clearCompletedButton = (
    <button onClick={clearCompletedFn} className="clearComp">
      Clear completed
    </button>
  );

  const activeTodos = todos.filter(todo => todo.is_completed !== true);

  let itemCount = todos.length;
  if (currentFilter === 'active') {
    itemCount = activeTodos.length;
  } else if (currentFilter === 'completed') {
    itemCount = todos.length - activeTodos.length;
  }

  return (
    <div className="footerList">
      <span> {itemCount} item{itemCount !== 1 ? "s" : ""}</span>

      <ul>
        <li onClick={(e) => {filterResultsHandler("all")}}>
          <a className={currentFilter === "all" ? "selected" : ""}>
            All
          </a>
        </li>

        <li onClick={(e) => {filterResultsHandler("active")}}>
          <a className={currentFilter === "active" ? "selected" : ""}>
            Active
          </a>
        </li>

        <li onClick={(e) => {filterResultsHandler("completed")}}>
          <a className={currentFilter === "completed" ? "selected" : ""}>
            Completed
          </a>
        </li>
      </ul>

      {clearCompletedButton}
    </div>
  );
};

export default TodoFilters;
