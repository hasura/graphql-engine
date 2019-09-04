import React, { Component, Fragment } from "react";

import TodoItem from "./TodoItem";
import TodoFilters from "./TodoFilters";

import { TodoPrivateListState } from './TodoTypeDefs';

class TodoPrivateList extends Component<any, TodoPrivateListState> {
  constructor(props: TodoPrivateListState) {
    super(props);

    this.state = {
      filter: "all",
      clearInProgress: false,
      todos: [
        {
          id: "1",
          title: "This is private todo 1",
          is_completed: true,
          is_public: false
        },
        {
          id: "2",
          title: "This is private todo 2",
          is_completed: false,
          is_public: false
        }
      ]
    };

    this.filterResults = this.filterResults.bind(this);
    this.clearCompleted = this.clearCompleted.bind(this);
  }

  filterResults(filter: string): void {
    this.setState({
      ...this.state,
      filter: filter
    });
  }

  clearCompleted() {}

  render() {
    let filteredTodos = this.state.todos;
    if (this.state.filter === "active") {
      filteredTodos = this.state.todos.filter(todo => todo.is_completed !== true);
    } else if (this.state.filter === "completed") {
      filteredTodos = this.state.todos.filter(todo => todo.is_completed === true);
    }

    const todoList = [] as any;
    filteredTodos.forEach((todo, index) => {
      todoList.push(
        <TodoItem
          key={'item'+index}
          index={index}
          todo={todo}
        />
      );
    });

    return (
      <Fragment>
        <div className="todoListWrapper">
          <ul>
            { todoList }
          </ul>
        </div>

        <TodoFilters
          todos={filteredTodos}
          currentFilter={this.state.filter}
          filterResultsFn={this.filterResults}
          clearCompletedFn={this.clearCompleted}
        />
      </Fragment>
    );
  }
}

export default TodoPrivateList;
