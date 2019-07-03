import React, { Component } from 'react';
import { GraphQLVoyager } from 'graphql-voyager';
import fetch from 'isomorphic-fetch';
import Endpoints from '../../Endpoints';
import '../../../node_modules/graphql-voyager/dist/voyager.css';
import './voyagerView.css';

class VoyagerView extends Component {
  introspectionProvider(query) {
    return fetch(Endpoints.graphQLUrl, {
      method: 'POST',
      headers: this.props.headers,
      body: JSON.stringify({ query: query }),
    }).then(response => response.json());
  }

  render() {
    return (
      <div>
        <GraphQLVoyager
          introspection={this.introspectionProvider.bind(this)}
          workerURI={
            'https://cdn.jsdelivr.net/npm/graphql-voyager@1.0.0-rc.27/dist/voyager.worker.min.js'
          }
        />
      </div>
    );
  }
}

const generatedVoyagerConnector = connect => {
  const mapStateToProps = state => {
    return {
      headers: state.tables.dataHeaders,
    };
  };
  return connect(mapStateToProps)(VoyagerView);
};

export default generatedVoyagerConnector;
