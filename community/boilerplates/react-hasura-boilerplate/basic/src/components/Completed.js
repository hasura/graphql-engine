import React, { Component } from 'react';
import { Subscription } from "react-apollo";
import { fetchTodosCompletedSubs } from '../queries/Queries';
import Todo from './Todo';
import '../App.css';

class Completed extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    const { user_id } = this.props;
    return (
      <Subscription subscription={fetchTodosCompletedSubs} variables={{ user_id }}>
      {
        ({ loading, error, data }) => {
          if(loading)
            return <p></p>
          if(error)
            return <p>Error </p>;

          return data.todo.map((t) => (
            <div className="todo-list-completed">
              <Todo t={t} user_id={user_id} key={t.id} />
            </div>
          ))
        }
      }
      </Subscription>
    )
  }
}

export default Completed;
