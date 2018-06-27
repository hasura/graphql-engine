import React, { Component } from 'react';
// import logo from './logo.svg';
import '../node_modules/graphiql/graphiql.css';

import GraphiQL from 'graphiql';
import fetch from 'isomorphic-fetch';

import {query, variables} from './graphiql-vars';

const {
    parse,
    buildASTSchema
} = require('graphql');

// const ravenUrl = process.env.RAVEN_URL || 'http://localhost:8080';
// const ravenUrl = window.location.hostname;

class App extends Component {

  render() {

      const graphQLFetcher = function(graphQLParams) {
          return fetch('/v1alpha1/graphql', {
              method: 'post',
              headers: { 'Content-Type': 'application/json'
                       },
              body: JSON.stringify(graphQLParams)
          }).then(response => response.json());
      };

      var content = <GraphiQL fetcher={graphQLFetcher} query={query} variables={variables}/>;

      return (
          <div className="react-container-graphql">
              {content}
          </div>
      );
  }
}

export default App;
