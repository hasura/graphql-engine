import React, { Component } from 'react';
import { Query } from "react-apollo";
import gql from "graphql-tag";
import Todo from './Todo';
import { FetchAllQuery } from '../queries/Queries';
import '../App.css';
import TodoLoader from '../Loaders/TodoLoader';

const Todos = () => (
  <Query query={FetchAllQuery}>
  {
    ({ loading, error, data }) => {
      if(loading)
        return <TodoLoader />
      if(error)
        return <p className="todo-list">Error </p>;

      if(data.todos.length == 0)
        return <h3 className="todo-list">No todos to complete :\/ </h3>;

      return data.todos.map((todo) => (
        <div key={todo.id} className="todo-list" data-toggle="tooltip" data-placement="left" title="click to mark completed">
          <Todo data={todo} />
        </div>
      ))
    }
  }
  </Query>
);

export default Todos;
