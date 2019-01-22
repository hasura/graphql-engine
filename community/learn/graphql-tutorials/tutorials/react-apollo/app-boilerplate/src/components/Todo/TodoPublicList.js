import React, { Component, Fragment } from "react";
import PropTypes from "prop-types";

import TodoItem from "./TodoItem";
import TodoFilters from "./TodoFilters";
import "../../styles/App.css";

class TodoPublicList extends Component {
  constructor() {
    super();

    this.state = {
      filter: "all",
      clearInProgress: false,
      olderTodosAvailable: true,
      newTodosCount: 0,
      todos: [
        {
          id: "1",
          text: "This is public todo 1",
          is_completed: true,
          is_public: true,
          user: {
            name: "someUser1"
          }
        },
        {
          id: "2",
          text: "This is public todo 2",
          is_completed: false,
          is_public: true,
          user: {
            name: "someUser2"
          }
        }
      ]
    };

    this.filterResults = this.filterResults.bind(this);
    this.clearCompleted = this.clearCompleted.bind(this);
    this.loadMoreClicked = this.loadMoreClicked.bind(this);
    this.loadOlderClicked = this.loadOlderClicked.bind(this);
  }

  filterResults(filter) {
    this.setState({
      ...this.state,
      filter: filter
    });
  }

  clearCompleted(type) {}

  loadMoreClicked() {}

  loadOlderClicked() {}

  render() {
    const { type } = this.props;

    let filteredTodos = this.state.todos;
    if (this.state.filter === "active") {
      filteredTodos = this.state.todos.filter(todo => todo.is_completed !== true);
    } else if (this.state.filter === "completed") {
      filteredTodos = this.state.todos.filter(todo => todo.is_completed === true);
    }

    const todoList = (
      <ul>
        {
          filteredTodos.map((todo, index) => {
            return (
              <TodoItem
                key={index}
                index={index}
                todo={todo}
                type={type}
              />
            );
          })
        }
      </ul>
    );

    let newTodosNotification = '';
    if (this.state.newTodosCount) {
      newTodosNotification = (
        <div className={"loadMoreSection"} onClick={this.loadMoreClicked}>
          You have {this.state.newTodosCount} new Todo{this.state.newTodosCount !== 1 ? "s" : ""}
        </div>
      );
    }

    const olderTodosMsg = (
      <div className={"loadMoreSection"} onClick={this.loadOlderClicked}>
        { this.state.olderTodosAvailable ? 'Load Older Todos' : 'No more Todos available'}
      </div>
    );

    return (
      <Fragment>
        <div className="todoListWrapper">
          { newTodosNotification }

          { todoList }

          { olderTodosMsg }
        </div>

        <TodoFilters
          todos={filteredTodos}
          type={type}
          currentFilter={this.state.filter}
          filterResultsFn={this.filterResults}
          clearCompletedFn={this.clearCompleted}
          clearInProgress={this.state.clearInProgress}
        />
      </Fragment>
    );
  }
}

TodoPublicList.propTypes = {
  type: PropTypes.string
};

export default TodoPublicList;
