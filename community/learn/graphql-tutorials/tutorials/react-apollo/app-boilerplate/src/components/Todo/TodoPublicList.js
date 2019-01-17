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
      showNew: false,
      showOlder: true,
      newTodosLength: 0,
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

    const data = [
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
    ];

    let filteredTodos = data;
    if (this.state.filter === "active") {
      filteredTodos = data.filter(todo => todo.is_completed !== true);
    } else if (this.state.filter === "completed") {
      filteredTodos = data.filter(todo => todo.is_completed === true);
    }

    const todoList = [];
    filteredTodos.forEach((todo, index) => {
      todoList.push(
        <TodoItem
          key={index}
          index={index}
          todo={todo}
          type={type}
        />
      );
    });

    // show new todo feed logic
    let newTodosMsg = '';
    if (this.state.showNew && this.state.newTodosLength) {
      newTodosMsg = (
        <div className={"loadMoreSection"} onClick={this.loadMoreClicked}>
          You have {this.state.newTodosLength} new Todo{this.state.newTodosLength !== 1 ? "s" : ""}
        </div>
      );
    }

    // show old todo history logic
    const olderTodosMsg = (
      <div className={"loadMoreSection"} onClick={this.loadOlderClicked}>
        {(!this.state.showOlder && this.state.todos.length) ? 'No more Todos available' : 'Load Older Todos'}
      </div>
    );

    return (
      <Fragment>
        <div className="todoListWrapper">

          { newTodosMsg }

          <ul>
            { todoList }
          </ul>

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
