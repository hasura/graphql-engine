import React, { Component } from "react"
import PropTypes from "prop-types";
import "../../styles/App.css";

class TodoItem extends Component {
  render() {
    const {index, todo, type} = this.props;

    let userAvatar = '';
    if (todo.user && todo.user.name) {
      userAvatar = (
        <div className="userInfoPublic" title={todo.user.name}>
          {todo.user.name.charAt(0).toUpperCase()}
        </div>
      );
    }

    const removeTodo = (e) => {
        e.preventDefault();
        e.stopPropagation();
    };

    const toggleTodo = () => {};

    return (
      <li>
        { todo.is_public ? userAvatar : '' }

        <div className="view">
          <div className="round">
            <input
              checked={todo.is_completed}
              type="checkbox"
              id={todo.id}
              onChange={toggleTodo}
            />
            <label htmlFor={todo.id}/>)
          </div>
        </div>

        <div className={"labelContent" + (todo.is_completed ? " completed" : '')}>
          <div data-test={type + "_" + index + "_" + todo.title}>
            {todo.title}
          </div>
        </div>

        <button
          className="closeBtn"
          onClick={removeTodo}
          data-test={"remove_" + type + "_" + index + "_" + todo.text}
        >
          x
        </button>
      </li>
    );
  }
}

TodoItem.propTypes = {
  todo: PropTypes.object.isRequired,
  type: PropTypes.string.isRequired
};

export default TodoItem;
