import React, { Component } from "react";

import TodoInput from "./TodoInput";
import TodoPublicList from "./TodoPublicList";
import "../../styles/App.css";

class TodoPublicWrapper extends Component {
  render() {
    // const userId = localStorage.getItem("auth0:id_token:sub");

    return (
      <div className="todoWrapper">
        <div className="sectionHeader">Public todos</div>

        <TodoInput type="public" />
        <TodoPublicList type="public" />
      </div>
    );
  }
}

export default TodoPublicWrapper;
