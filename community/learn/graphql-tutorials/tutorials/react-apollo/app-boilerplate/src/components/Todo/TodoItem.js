import React from "react";
import PropTypes from "prop-types";
import "../../styles/App.css";

const TodoItem = ({
  index,
  todo,
  type,
  userId,
  completePublicTodoClicked,
  deletePublicTodoClicked
}) => (
    <li
      onClick={() => {
      }}
    >
      {todo.is_public ? (
        <div className="userInfoPublic" title={todo.user.name}>
          {todo.user.name.charAt(0).toUpperCase()}
        </div>
      ) : null}
      <div className="view">
        {todo.is_completed ? (
          <div className="round">
            <input
              checked={true}
              type="checkbox"
              id={todo.id}
              onChange={() => {
              }}
            />
            <label htmlFor={todo.id} />
          </div>
        ) : (
          <div className="round">
            <input
              type="checkbox"
              checked={false}
              id={todo.id}
              onChange={() => {
              }}
            />
            <label htmlFor={todo.id} />)
          </div>
        )}
      </div>
      <div className="labelContent">
        {todo.is_completed ? (
          <strike className="todoLabel">
            <div data-test={type + "_" + index + "_" + todo.text}>
              {todo.text}
            </div>
          </strike>
        ) : (
          <div data-test={type + "_" + index + "_" + todo.text}>
            {todo.text}
          </div>
        )}
      </div>
      <button
        className="closeBtn"
        data-test={"remove_" + type + "_" + index + "_" + todo.text}
        onClick={e => {
          e.preventDefault();
          e.stopPropagation();
        }}
      >
        x
      </button>
    </li>
  );

TodoItem.propTypes = {
  todo: PropTypes.object.isRequired,
  type: PropTypes.string,
  userId: PropTypes.string
};

export default TodoItem;
