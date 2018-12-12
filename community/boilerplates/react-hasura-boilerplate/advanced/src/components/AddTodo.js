import React, { Component } from 'react';
import { Query } from "react-apollo";
import { FetchAllQuery, AddTodoQuery, CompletedTodos, getCompletedQuery } from '../queries/Queries';
import { Mutation } from "react-apollo";
import '../App.css';

class AddTodo extends Component {
  getUser() {
    const sub = localStorage.getItem('sub');
    return sub;
  }

  addnewTodo(addTodo, e) {
    if(e.which === 13) {
      const user_id = this.getUser();
      addTodo({
        variables: { todo_name: e.target.value, user_id: user_id },
        refetchQueries: [{ query: FetchAllQuery }, { query: getCompletedQuery }]
      });
      e.target.value = "";
    }
  }
  render () {
    return (
      <Mutation mutation={AddTodoQuery}>
        {
          (addTodo, { data }) => (
            <div>
              <input type="text" className="form-control inp-todo" placeholder="add your todo..." onKeyPress={this.addnewTodo.bind(this, addTodo)} />
              <h4>( Click todo to mark as completed )</h4>
            </div>
          )
        }
      </Mutation>
    );
  }
}

export default AddTodo;
