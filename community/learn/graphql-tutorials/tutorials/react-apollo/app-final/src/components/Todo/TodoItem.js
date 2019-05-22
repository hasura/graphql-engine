import React from 'react';
import {withApollo} from 'react-apollo';
import gql from 'graphql-tag';
import {GET_MY_TODOS} from './TodoPrivateList';

const TodoItem = ({index, todo, client}) => {

  const REMOVE_TODO = gql`
    mutation removeTodo ($id: Int!) {
      delete_todos(where: {id: {_eq: $id}}) {
        affected_rows
      }
    }
  `;

  const removeTodo = (e) => {
    e.preventDefault();
    e.stopPropagation();
    client.mutate({
      mutation: REMOVE_TODO,
      variables: {id: todo.id},
      optimisticResponse: {},
      update: (cache, {}) => {
        const existingTodos = cache.readQuery({ query: GET_MY_TODOS });
        const newTodos = existingTodos.todos.filter(t => (t.id !== todo.id));
        cache.writeQuery({
          query: GET_MY_TODOS,
          data: {todos: newTodos}
        });
      }
    });
  };

  const TOGGLE_TODO = gql`
    mutation toggleTodo ($id: Int!, $isCompleted: Boolean!) {
      update_todos(where: {id: {_eq: $id}}, _set: {is_completed: $isCompleted}) {
        affected_rows
      }
    }
  `;

  const toggleTodo = () => {
    client.mutate({
      mutation: TOGGLE_TODO,
      variables: {id: todo.id, isCompleted: !todo.is_completed},
      optimisticResponse: {},
      update: (cache) => {
        const existingTodos = cache.readQuery({ query: GET_MY_TODOS });
        const newTodos = existingTodos.todos.map(t => {
          if (t.id === todo.id) {
            return({...t, is_completed: !t.is_completed});
          } else {
            return t;
          }
        });
        cache.writeQuery({
          query: GET_MY_TODOS,
          data: {todos: newTodos}
        });
      }
    });
  };

  return (
    <li>
      <div className="view">
        <div className="round">
          <input
            checked={todo.is_completed}
            type="checkbox"
            id={todo.id}
            onChange={toggleTodo}
          />
          <label htmlFor={todo.id}/>
        </div>
      </div>

      <div className={"labelContent" + (todo.is_completed ? " completed" : '')}>
        <div>
          {todo.title}
        </div>
      </div>

      <button className="closeBtn" onClick={removeTodo}>
        x
      </button>
    </li>
  );
};

export default withApollo(TodoItem);
