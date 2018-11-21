import React from "react";
import PropTypes from "prop-types";

const TodoFilters = ({
  todos,
  currentFilter,
  type,
  filterResults,
  clearCompleted,
  clearInProgress
}) => {
  const activeTodos = todos.filter(todo => todo.is_completed !== true);
  return (
    <div className="footerList">
      <span> {activeTodos.length} items left </span>
      <ul>
        <li onClick={() => filterResults("all")}>
          <a className={currentFilter === "all" ? "selected" : ""}>All</a>
        </li>
        <li onClick={() => filterResults("active")}>
          <a
            className={
              currentFilter === "active"
                ? "selected removePaddLeft"
                : "removePaddLeft"
            }
          >
            Active
          </a>
        </li>
        <li onClick={() => filterResults("completed")}>
          <a
            className={
              currentFilter === "completed"
                ? "selected removePaddLeft"
                : "removePaddLeft"
            }
          >
            Completed
          </a>
        </li>
      </ul>
      {type === "private" ? (
        <button onClick={() => clearCompleted(type)} className="clearComp">
          {clearInProgress ? "Clearing" : "Clear completed"}
        </button>
      ) : null}
    </div>
  );
};

TodoFilters.propTypes = {
  todos: PropTypes.array.isRequired,
  userId: PropTypes.string,
  type: PropTypes.string,
  currentFilter: PropTypes.string,
  filterResults: PropTypes.func
};

export default TodoFilters;
