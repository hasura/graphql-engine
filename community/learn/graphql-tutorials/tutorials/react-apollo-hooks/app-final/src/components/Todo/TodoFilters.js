/* eslint-disable jsx-a11y/anchor-is-valid */
import React from 'react';

const TodoFilters = ({
  todos,
  currentFilter,
  filterResultsFn,
  clearCompletedFn
}) => {
  const filterResultsHandler = (filter) => {
    return () => {
      filterResultsFn(filter);
    }
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
        <li onClick={filterResultsHandler("all")}>
          <a className={currentFilter === "all" ? "selected" : ""}>
            All
          </a>
        </li>

        <li onClick={filterResultsHandler("active")}>
          <a className={currentFilter === "active" ? "selected" : ""}>
            Active
          </a>
        </li>

        <li onClick={filterResultsHandler("completed")}>
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
