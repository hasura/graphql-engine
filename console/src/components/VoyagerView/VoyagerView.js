import React, { Component } from 'react';
import { GraphQLVoyager } from 'graphql-voyager';
import fetch from 'isomorphic-fetch';
import Endpoints from '../../Endpoints';
import '../../../node_modules/graphql-voyager/dist/voyager.css';
import './voyagerView.css';

console.log(GraphQLVoyager);

class VoyagerView extends Component {
  introspectionProvider(query) {
    return fetch(Endpoints.graphQLUrl, {
      method: 'post',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ query: query }),
    }).then(response => response.json());
  }

  render() {
    return (
      <div>
        <GraphQLVoyager
          introspection={this.introspectionProvider}
          workerURI={
            'https://cdn.jsdelivr.net/npm/graphql-voyager@1.0.0-rc.27/dist/voyager.worker.min.js'
          }
        />
      </div>
    );
  }
}

export default VoyagerView;
