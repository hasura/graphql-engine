import React, { Component } from 'react';
import { Mutation } from "react-apollo";
import { addTodoQuery } from '../queries/Queries';
import Navbar from './Navbar';
import Mark from './Mark';
import UnCompletedTodos from './UnCompletedTodos';
import Completed from './Completed';
import $ from 'jquery';
import '../App.css';

class Dashboard extends Component {
  constructor(props) {
    super(props);
  }

  addnewTodo(addTodo, e) {
    if(e.which === 13) {
      addTodo({
        variables: { data: e.target.value, user_id: this.props.match.params.userID },
      })
      e.target.value = "";
    }
  }

  render() {
    const { name, userID } = this.props.match.params;
    return (
      <div>
        <Navbar name={name} />
        <Mutation mutation={addTodoQuery} >
          {
            (addTodo, { data }) => (
              <div className="container login-main">
                <input placeholder="add a todo ..." className="input-bar form-control" onKeyPress={this.addnewTodo.bind(this, addTodo)} />
                <br />
                <p>( click todo to mark completed )</p>
                <br />
              </div>
            )
          }
        </Mutation>
        <UnCompletedTodos user_id={userID} />
        <Completed user_id={userID} />
      </div>
    );
  }
}

export default Dashboard;
