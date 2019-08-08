import React, { Component, Fragment } from 'react';

import TaskItem from "./TaskItem";

class TodoPublicList extends Component {
  constructor() {
    super();

    this.state = {
      olderTodosAvailable: true,
      newTodosCount: 1,
      todos: [
        {
          id: "1",
          title: "This is public todo 1",
          user: {
            name: "someUser1"
          }
        },
        {
          id: "2",
          title: "This is public todo 2",
          is_completed: false,
          is_public: true,
          user: {
            name: "someUser2"
          }
        },
        {
          id: "3",
          title: "This is public todo 3",
          user: {
            name: "someUser3"
          }
        },
        {
          id: "4",
          title: "This is public todo 4",
          user: {
            name: "someUser4"
          }
        }
      ]
    };

    this.loadNew = this.loadNew.bind(this);
    this.loadOlder = this.loadOlder.bind(this);
  }

  loadNew() {}

  loadOlder() {}

  render() {

    let todos = this.state.todos;

    const todoList = (
      <ul>
        {
          todos.map((todo, index) => {
            return (
              <TaskItem
                key={index}
                index={index}
                todo={todo}
              />
            );
          })
        }
      </ul>
    );

    let newTodosNotification = '';
    if (this.state.newTodosCount) {
      newTodosNotification = (
        <div className={"loadMoreSection"} onClick={this.loadNew}>
          New tasks have arrived! ({this.state.newTodosCount.toString()})
        </div>
      );
    }

    const olderTodosMsg = (
      <div className={"loadMoreSection"} onClick={this.loadOlder}>
        { this.state.olderTodosAvailable ? 'Load older tasks' : 'No more public tasks!'}
      </div>
    );

    return (
      <Fragment>
        <div className="todoListWrapper">
          { newTodosNotification }

          { todoList }

          { olderTodosMsg }
        </div>
      </Fragment>
    );
  }
}

export default TodoPublicList;
