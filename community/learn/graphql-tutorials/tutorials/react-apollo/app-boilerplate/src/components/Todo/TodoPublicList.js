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
      dataLength: 0,
      showNew: false,
      showOlder: true,
      newTodosLength: 0,
      limit: 5,
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
  }

  filterResults(filter) {
    this.setState({
      ...this.state,
      filter: filter
    });
  }

  clearCompleted(type) {}

  render() {
    const { type } = this.props;

    // show new todo feed logic
    let showNewTodos = '';
    if (this.state.showNew && this.state.newTodosLength) {
      showNewTodos = (
        <div className={"loadMoreSection"} onClick={this.loadMoreClicked}>
          You have {this.state.newTodosLength} new{" "}
          {this.state.newTodosLength > 1 ? "todos" : "todo"}
        </div>
      );
    }

    // show old todo history logic
    let showOlderTodos = (
      <div className={"loadMoreSection"} onClick={this.loadOlderClicked}>
        Load Older Todos
      </div>
    );

    if (!this.state.showOlder && this.state.todos.length) {
      showOlderTodos = (
        <div className={"loadMoreSection"} onClick={this.loadOlderClicked}>
          No more todos available
        </div>
      );
    }

    // apply client side filters for displaying todos
    let filteredTodos = this.state.todos;
    if (this.state.filter === "active") {
      filteredTodos = this.state.todos.filter(todo => todo.is_completed !== true);
    } else if (this.state.filter === "completed") {
      filteredTodos = this.state.todos.filter(todo => todo.is_completed === true);
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
    })

    return (
      <Fragment>
        <div className="todoListWrapper">

          { showNewTodos }

          <ul>
            { todoList }
          </ul>

          { showOlderTodos }
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
