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

  return (
    <div className="footerList">
      <span> {activeTodos.length} item{activeTodos.length !== 1 ? "s" : ""}</span>

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
