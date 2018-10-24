import React, { Component } from 'react';
import { Voyager } from 'graphql-voyager';
import fetch from 'isomorphic-fetch';
import Endpoints from '../../Endpoints';
import hasuraconfig from '../../../hasuraconfig';
import '../../../node_modules/graphql-voyager/dist/voyager.css';
import './voyagerView.css';

class VoyagerView extends Component {
  introspectionProvider(query) {
    return fetch(Endpoints.graphQLUrl, {
      method: 'post',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ query: query }),
    }).then(response => response.json());
  }

  render() {
    const rootType = this.props.params.root;
    return (
      <Voyager
        introspection={this.introspectionProvider}
        displayOptions={{ rootType: rootType }}
        workerURI={
          'http://' +
          hasuraconfig.hmrHost +
          ':' +
          hasuraconfig.hmrPort +
          hasuraconfig.webpackPrefix +
          'voyager.worker.js'
        }
      />
    );
  }
}

export default VoyagerView;
