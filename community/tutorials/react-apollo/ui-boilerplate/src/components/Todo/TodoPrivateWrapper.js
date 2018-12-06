import React, { Component } from "react";
import TodoPrivateList from "./TodoPrivateList";
import TodoInput from "./TodoInput";
import "../../styles/App.css";

class TodoPrivateWrapper extends Component {
  render() {
    const userId = localStorage.getItem("auth0:id_token:sub");
    return (
      <div className="todoWrapper">
        <TodoInput userId={userId} type="private" />
        <TodoPrivateList userId={userId} type="private" />
      </div>
    );
  }
}

export default TodoPrivateWrapper;
