import React, { Component } from "react";

import TodoInput from "./TodoInput";
import TodoPrivateList from "./TodoPrivateList";
import "../../styles/App.css";

class TodoPrivateWrapper extends Component {
  render() {
    // const userId = localStorage.getItem("auth0:id_token:sub");

    return (
      <div className="todoWrapper">
        <div className="sectionHeader">Personal todos</div>

        <TodoInput type="private" />
        <TodoPrivateList type="private" />
      </div>
    );
  }
}

export default TodoPrivateWrapper;
