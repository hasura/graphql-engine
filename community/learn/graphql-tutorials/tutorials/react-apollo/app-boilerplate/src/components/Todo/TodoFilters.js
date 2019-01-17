import React, { Component } from "react";
import PropTypes from "prop-types";

import "../../styles/App.css";

class TodoFilters extends Component {
  render() {
    const { todos, currentFilter, type, filterResultsFn, clearCompletedFn, clearInProgress } = this.props;

    const filterResultsHandler = (filter) => {
      return () => {
        filterResultsFn(filter);
      }
    };

    const clearCompletedHandler = () => {
      clearCompletedFn();
    };

    const clearCompletedButton = (
      <button onClick={clearCompletedHandler} className="clearComp">
        { clearInProgress ? "Clearing..." : "Clear completed" }
      </button>
    );

    const activeTodos = todos.filter(todo => todo.is_completed !== true);

    return (
      <div className="footerList">
        <span> {activeTodos.length} item{activeTodos.length !== 1 ? "s" : ""} left </span>

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

        {type === "private" ? clearCompletedButton : ''}
      </div>
    );
  }
}

TodoFilters.propTypes = {
  todos: PropTypes.array.isRequired,
  type: PropTypes.string.isRequired,
  currentFilter: PropTypes.string.isRequired,
  filterResultsFn: PropTypes.func.isRequired,
  clearCompletedFn: PropTypes.func.isRequired,
  clearInProgress: PropTypes.bool.isRequired
};

export default TodoFilters;
