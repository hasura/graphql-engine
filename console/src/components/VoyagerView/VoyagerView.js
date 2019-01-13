import React, { Component } from 'react';
import { Voyager } from 'graphql-voyager';
import ReactDOM from 'react-dom';
import fetch from 'isomorphic-fetch';
import Endpoints from '../../Endpoints';
import '../../../node_modules/graphql-voyager/dist/voyager.css';
import './voyagerView.css';

class VoyagerView extends Component {
  introspectionProvider(query) {
    return fetch(Endpoints.graphQLUrl, {
      method: 'post',
      headers: {
        'Content-Type': 'application/json',
        'x-hasura-access-key': 'abcd',
      },
      body: JSON.stringify({ query: query }),
    }).then(response => response.json());
  }

  render() {
    const rootType = this.props.params.root;
    return ReactDOM.createPortal(
      <Voyager
        introspection={this.introspectionProvider}
        displayOptions={{ rootType: rootType }}
        workerURI={
          'https://storage.googleapis.com/hasura-graphql-engine/console/assets/voyager.worker.js'
        }
      />,
      document.getElementById('voyager-root')
    );
  }
}

export default VoyagerView;
