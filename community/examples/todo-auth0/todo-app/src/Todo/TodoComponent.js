import React from 'react';
import './Todo.css';
import TodoInput from './TodoInput';
import TodoList from './TodoList';

export default class TodoComponent extends React.Component {

  render() {
    const userId = localStorage.getItem('auth0:id_token:sub');
    return (
      <div className="parentContainer">
        <h1 className="header">Todos</h1>
        <TodoInput userId={userId}/>
        <TodoList />
      </div>
    )
  }
}
