import React, { Component } from 'react';
import { Mutation } from "react-apollo";
import gql from "graphql-tag";
import { markCompletedQuery, deleteQuery } from '../queries/Queries';
import $ from 'jquery'
import '../App.css';


class Todo extends Component {
  constructor(props) {
    super(props);
    this.state = {
      comp: 0,
      created_at: "",
      updated_at: "",
    }
  }

  markCompleted(completeTodo, id, e) {
    $("#" + id + " h3").css({ 'text-decoration': 'line-through'});
    var date = new Date();
    completeTodo({
      variables: { id: id, updated_at: date },
    })
  }

  delete(deleteTodo, id, e) {
    deleteTodo({
      variables: { id: id },
    })
  }

  func(is_completed, created_at, updated_at) {
    if(is_completed === false) {
      return <span><b>Created</b> at {created_at.slice(12, 16)} on {created_at.slice(0, 10)}</span>;
    }
    else {
      return <div>
                <span><b>Created</b> at {created_at.slice(12, 16)} on {created_at.slice(0, 10)}</span>
                &nbsp;&nbsp;-&nbsp;&nbsp;
                <span><b>Completed</b> at {updated_at.slice(12, 16)} on {updated_at.slice(0, 10)}</span>
             </div>;
    }
  }

  render() {
    const { t } = this.props;
    return (
      <Mutation mutation={markCompletedQuery} >
        {
          (completeTodo, { data }) => (
            <div className="todos" onClick={this.markCompleted.bind(this, completeTodo, t.id)} id={t.id}>
              <Mutation mutation={deleteQuery} >
                {
                  (deleteTodo, { data }) => (
                    <i className="fas fa-times-circle cross" onClick={this.delete.bind(this, deleteTodo, t.id)}></i>
                  )
                }
              </Mutation>
              <h3>{t.data}</h3>
              {this.func(t.is_completed, t.created_at, t.updated_at)}
            </div>
          )
        }
      </Mutation>
    )
  }
}

export default Todo;
