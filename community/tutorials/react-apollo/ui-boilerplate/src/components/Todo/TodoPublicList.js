import React, { Component, Fragment } from "react";
import PropTypes from "prop-types";
import TodoItem from "./TodoItem";
import TodoFilters from "./TodoFilters";

class TodoPublicList extends Component {
  constructor() {
    super();
    this.state = {
      filter: "all",
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
  }
  filterResults(type) {
    this.setState({ ...this.state, filter: type });
  }
  render() {
    const { userId, type } = this.props;

    // apply client side filters for displaying todos
    let finalTodos = this.state.todos;
    if (this.state.filter === "active") {
      finalTodos = this.state.todos.filter(todo => todo.is_completed !== true);
    } else if (this.state.filter === "completed") {
      finalTodos = this.state.todos.filter(todo => todo.is_completed === true);
    }

    // show new todo feed logic
    let showNewTodos = null;
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

    return (
      <Fragment>
        <div className="todoListwrapper">
          {showNewTodos}
          <ul>
            {finalTodos.map((todo, index) => {
              return (
                <TodoItem
                  key={index}
                  index={index}
                  todo={todo}
                  type={type}
                  userId={userId}
                  client={this.props.client}
                  deletePublicTodoClicked={this.deletePublicTodoClicked}
                  completePublicTodoClicked={this.completePublicTodoClicked}
                />
              );
            })}
          </ul>
          {showOlderTodos}
        </div>
        <TodoFilters
          todos={this.state.todos}
          userId={userId}
          type={type}
          currentFilter={this.state.filter}
          filterResults={this.filterResults}
        />
      </Fragment>
    );
  }
}

TodoPublicList.propTypes = {
  userId: PropTypes.string,
  type: PropTypes.string
};

export default TodoPublicList;
