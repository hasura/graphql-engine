import React from 'react';
import { Query } from "react-apollo";
import Todo from './Todo';
import {
  QUERY_TODO,
} from './graphQueries/todoQueries';

const TodoList = () => (
  <Query query={QUERY_TODO}>
    {({loading, error, data}) => {
      if (loading) {
        return (
          <div>Loading. Please wait...</div>
        );
      }
      if (error) {
        return (
          <div>{""}</div>
        );
      }
      return (
        <div className="parentContainer">
          <ul className="todoList">
          {
            data.todo.map((todo, index) => {
              return (
                <Todo key={index} todo={todo} />
              );
            })
          }
          </ul>
        </div>
      )
    }}
  </Query>
);

export default TodoList;
