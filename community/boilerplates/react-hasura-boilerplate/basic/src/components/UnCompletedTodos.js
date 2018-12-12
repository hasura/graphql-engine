import React, { Component } from 'react';
import { Subscription } from "react-apollo";
import { fetchTodosUncompletedSubs } from '../queries/Queries';
import Todo from './Todo';
import '../App.css';

class UnCompletedTodos extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    const { user_id } = this.props;
    return (
      <Subscription subscription={fetchTodosUncompletedSubs} variables={{ user_id }}>
      {
        ({ loading, error, data }) => {
          if(loading)
            return <p>loading ...</p>
          if(error)
            return <p>Error </p>;

          return data.todo.map((t) => (
            <div className="todo-list">
              <Todo t={t} user_id={user_id} key={t.id} />
            </div>
          ))
        }
      }
      </Subscription>
    )
  }
}

export default UnCompletedTodos;
