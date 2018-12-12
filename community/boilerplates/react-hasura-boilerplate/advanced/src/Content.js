import React, { Component } from 'react';
import ApolloClient from "apollo-boost";
import { ApolloProvider } from "react-apollo";
import Todos from './components/Todos';
import AddTodo from './components/AddTodo';
import CompletedTodos from './components/CompletedTodos';
import Navbar from './components/Navbar';
import { vars } from './env';
import './App.css';

const ACCESS_TOKEN = localStorage.getItem('access_token');

const client = new ApolloClient({
  uri: vars.GRAPHQL_ENDPOINT,
  headers: {
    Authorization: `Bearer ${ACCESS_TOKEN}`,
  }
})

class Content extends Component {
  render() {
    const { isAuthenticated } = this.props.auth;
    return (
      isAuthenticated() && (
      <ApolloProvider client={client}>
        <div className="App container-fluid">
          <h1 className="title">todos</h1>
          <div className="col-md-2"></div>
          <div className="col-md-8">
            <AddTodo />
            <Todos />
            <CompletedTodos />
          </div>
          <div className="col-md-2"></div>
        </div>
      </ApolloProvider>
      )
    );
  }
}

export default Content;
