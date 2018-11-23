import React from "react";
import PropTypes from "prop-types";
import { Mutation } from "react-apollo";
import "../../styles/App.css";

import {
  QUERY_PRIVATE_TODO,
  QUERY_PUBLIC_TODO,
  MUTATION_TODO_UPDATE,
  MUTATION_TODO_DELETE
} from "./TodoQueries";

const handleTodoToggle = (
  toggleTodo,
  todo,
  type,
  userId,
  completePublicTodoClicked
) => {
  toggleTodo({
    variables: {
      todoId: todo.id,
      set: {
        is_completed: !todo.is_completed
      }
    },
    update: (cache, { data: { update_todo } }) => {
      // eslint-disable-line
      const query = type === "private" ? QUERY_PRIVATE_TODO : QUERY_PUBLIC_TODO;
      if (type === "private") {
        const data = cache.readQuery({
          query: query,
          variables: { userId: userId }
        });
        const toggledTodo = data.todos.find(t => t.id === todo.id);
        toggledTodo.is_completed = !todo.is_completed;
        cache.writeQuery({
          query: query,
          variables: {
            userId: userId
          },
          data
        });
      } else if (type === "public") {
        completePublicTodoClicked(todo);
      }
    }
  });
};

const handleTodoDelete = (
  deleteTodo,
  todo,
  type,
  userId,
  deletePublicTodoClicked
) => {
  deleteTodo({
    variables: {
      todoId: todo.id
    },
    update: (cache, { data: { update_todo } }) => {
      // eslint-disable-line
      const query = type === "private" ? QUERY_PRIVATE_TODO : QUERY_PUBLIC_TODO;
      if (type === "private") {
        const data = cache.readQuery({
          query: query,
          variables: { userId: userId }
        });
        data.todos = data.todos.filter(t => {
          return t.id !== todo.id;
        });
        cache.writeQuery({
          query: query,
          variables: {
            userId: userId
          },
          data
        });
      } else if (type === "public") {
        deletePublicTodoClicked(todo);
      }
    }
  });
};

const TodoItem = ({
  index,
  todo,
  type,
  userId,
  completePublicTodoClicked,
  deletePublicTodoClicked
}) => (
  <Mutation mutation={MUTATION_TODO_UPDATE}>
    {updateTodo => {
      return (
        <Mutation mutation={MUTATION_TODO_DELETE}>
          {deleteTodo => {
            return (
              <li
                onClick={() => {
                  handleTodoToggle(
                    updateTodo,
                    todo,
                    type,
                    userId,
                    completePublicTodoClicked
                  );
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
                          handleTodoToggle(
                            updateTodo,
                            todo,
                            type,
                            userId,
                            completePublicTodoClicked
                          );
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
                          handleTodoToggle(
                            updateTodo,
                            todo,
                            type,
                            userId,
                            completePublicTodoClicked
                          );
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
                    handleTodoDelete(
                      deleteTodo,
                      todo,
                      type,
                      userId,
                      deletePublicTodoClicked
                    );
                  }}
                >
                  x
                </button>
              </li>
            );
          }}
        </Mutation>
      );
    }}
  </Mutation>
);

TodoItem.propTypes = {
  todo: PropTypes.object.isRequired,
  type: PropTypes.string,
  userId: PropTypes.string
};

export default TodoItem;
