import React, { useState } from "react";
import { useMutation } from "@apollo/react-hooks";
import gql from "graphql-tag";
import { GET_MY_TODOS } from "./TodoPrivateList";

const ADD_TODO = gql`
  mutation($todo: String!, $isPublic: Boolean!) {
    insert_todos(objects: { title: $todo, is_public: $isPublic }) {
      affected_rows
      returning {
        id
        title
        created_at
        is_completed
      }
    }
  }
`;

const TodoInput = ({ isPublic = false }) => {
  let input;

  const [todoInput, setTodoInput] = useState("");

  const updateCache = (cache, { data }) => {
    // If this is for the public feed, do nothing
    if (isPublic) {
      return null;
    }

    // Fetch the todos from the cache
    const existingTodos = cache.readQuery({
      query: GET_MY_TODOS
    });

    // Add the new todo to the cache
    const newTodo = data.insert_todos.returning[0];
    cache.writeQuery({
      query: GET_MY_TODOS,
      data: { todos: [newTodo, ...existingTodos.todos] }
    });
  };

  const resetInput = () => {
    setTodoInput("");
  };

  const [addTodo, { loading }] = useMutation(ADD_TODO, {
    update: updateCache,
    onCompleted: resetInput
  });

  return (
    <form
      className="formInput"
      onSubmit={e => {
        e.preventDefault();
        addTodo({ variables: { todo: todoInput, isPublic } });
      }}
    >
      <input
        className="input"
        value={todoInput}
        placeholder="What needs to be done?"
        ref={n => (input = n)}
        onChange={e => setTodoInput(e.target.value)}
      />
      <i className="inputMarker fa fa-angle-right" />
    </form>
  );
};

export default TodoInput;
