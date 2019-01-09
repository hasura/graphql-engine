import React, { Component, Fragment } from "react";
import PropTypes from "prop-types";
import TodoItem from "./TodoItem";
import TodoFilters from "./TodoFilters";
import {
  SUBSCRIPTION_TODO_PUBLIC_LIST,
  QUERY_PUBLIC_TODO,
  QUERY_FEED_PUBLIC_TODO,
  QUERY_FEED_PUBLIC_OLD_TODO
} from "./TodoQueries";

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
      todos: []
    };
    this.deletePublicTodoClicked = this.deletePublicTodoClicked.bind(this);
    this.completePublicTodoClicked = this.completePublicTodoClicked.bind(this);
    this.loadMoreClicked = this.loadMoreClicked.bind(this);
    this.loadOlderClicked = this.loadOlderClicked.bind(this);
    this.filterResults = this.filterResults.bind(this);
  }
  componentDidMount() {
    const { client } = this.props;
    const _this = this;
    // query for public todos
    client
      .query({
        query: QUERY_PUBLIC_TODO,
        variables: { todoLimit: this.state.limit }
      })
      .then(data => {
        this.setState({ todos: data.data.todos });
        const latestTodoId = data.data.todos.length
          ? data.data.todos[0].id
          : null;
        // start a subscription
        client
          .subscribe({
            query: SUBSCRIPTION_TODO_PUBLIC_LIST,
            variables: { todoId: latestTodoId } // update subscription when todoId changes
          })
          .subscribe({
            next(data) {
              if (data.data.todos.length) {
                _this.setState({
                  ...this.state,
                  showNew: true,
                  newTodosLength:
                    _this.state.newTodosLength + data.data.todos.length
                });
              }
            },
            error(err) {
              console.error("err", err);
            }
          });
      });
  }
  filterResults(type) {
    this.setState({ filter: type });
  }
  loadMoreClicked() {
    const { client } = this.props;
    this.setState({ ...this.state, showNew: false, newTodosLength: 0 });
    client
      .query({
        query: QUERY_FEED_PUBLIC_TODO,
        variables: {
          todoId: this.state.todos.length ? this.state.todos[0].id : null
        }
      })
      .then(data => {
        if (data.data.todos.length) {
          const mergedTodos = data.data.todos.concat(this.state.todos);
          // update state with new todos
          this.setState({ ...this.state, todos: mergedTodos });
        }
      });
  }
  loadOlderClicked() {
    const { client } = this.props;
    client
      .query({
        query: QUERY_FEED_PUBLIC_OLD_TODO,
        variables: {
          todoId: this.state.todos.length
            ? this.state.todos[this.state.todos.length - 1].id
            : null
        }
      })
      .then(data => {
        if (data.data.todos.length) {
          const mergedTodos = this.state.todos.concat(data.data.todos);
          // update state with new todos
          this.setState({ ...this.state, todos: mergedTodos });
        } else {
          this.setState({ ...this.state, showOlder: false });
        }
      });
  }
  deletePublicTodoClicked(deletedTodo) {
    const finalTodos = this.state.todos.filter(t => {
      return t.id !== deletedTodo.id;
    });
    this.setState({ ...this.state, todos: finalTodos });
  }
  completePublicTodoClicked(completedTodo) {
    const finalTodos = this.state.todos.filter(t => {
      if (t.id === completedTodo.id) {
        t.is_completed = !t.is_completed;
        return t;
      }
      return t;
    });
    this.setState({ ...this.state, todos: finalTodos });
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
  client: PropTypes.object,
  type: PropTypes.string
};

export default TodoPublicList;
