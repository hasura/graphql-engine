import React, { Component } from "react";
import TodoPrivateList from "./TodoPrivateList";
import TodoInput from "./TodoInput";
import "../../styles/App.css";

class TodoPrivateWrapper extends Component {
  render() {
    return (
      <div className="todoWrapper">
        <TodoInput userId={this.props.userId} type="private" />
        <TodoPrivateList
          userId={this.props.userId}
          client={this.props.client}
          type="private"
        />
      </div>
    );
  }
}

export default TodoPrivateWrapper;
