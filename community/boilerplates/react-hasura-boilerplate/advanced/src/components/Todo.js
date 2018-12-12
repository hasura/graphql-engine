import React, { Component } from 'react';
import $ from 'jquery';
import { Mutation } from "react-apollo";
import { FetchAllQuery, MarkCompletedQuery, getCompletedQuery, deleteQuery } from '../queries/Queries';
import '../App.css';

class Todo extends Component {
  constructor(props) {
    super(props);
  }

  completedTask(id, mark, e) {
    $("#" + id + " h3").css({ 'text-decoration': 'line-through'});
    mark({
      variables: { id: id },
      refetchQueries: [{ query: FetchAllQuery }, { query: getCompletedQuery }]
    })
  }

  deleteTodo(id, del, e) {
    del({
      variables: { id: id },
      refetchQueries: [{ query: FetchAllQuery }, { query: getCompletedQuery }]
    })
  }

  render() {
    return (
      <Mutation mutation={MarkCompletedQuery}>
        {
          (mark, { data }) => (
            <div className="todos" onClick={this.completedTask.bind(this, this.props.data.id, mark)} id={this.props.data.id}>
              <Mutation mutation={deleteQuery}>
                {
                  (del, { data1 }) => (
                    <div>
                      <i className="fas fa-times-circle cross" data-toggle="tooltip" data-placement="top" title="click to delete" onClick={this.deleteTodo.bind(this, this.props.data.id, del)}></i>
                      <h3>{`${this.props.data.todo_name}`}</h3>
                    </div>
                  )
                }
              </Mutation>
            </div>
          )
        }
      </Mutation>
    )
  }
}

export default Todo;
