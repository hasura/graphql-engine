import React, { Component } from 'react';
import { Connect } from 'react-redux';
import { GraphQLVoyager } from 'graphql-voyager';
import fetch from 'isomorphic-fetch';
import Endpoints from '../../../Endpoints';
import VoyagerViewErrorBoundary from './VoyagerViewErrorBoundary';
import { ReduxState } from '../../../types';
import '../../../../node_modules/graphql-voyager/dist/voyager.css';
import './voyagerView.css';

interface VoyagerViewProps {
  headers: Headers | Record<string, string>;
}

class VoyagerView extends Component<VoyagerViewProps> {
  introspectionProvider = (query: string) => {
    return fetch(Endpoints.graphQLUrl, {
      method: 'POST',
      headers: this.props.headers,
      body: JSON.stringify({ query }),
    }).then(response => response.json());
  };

  render() {
    return (
      <VoyagerViewErrorBoundary>
        <GraphQLVoyager
          introspection={this.introspectionProvider}
          workerURI="https://cdn.jsdelivr.net/npm/graphql-voyager@1.0.0-rc.27/dist/voyager.worker.min.js"
        />
      </VoyagerViewErrorBoundary>
    );
  }
}

const generatedVoyagerConnector = (connect: Connect) => {
  const mapStateToProps = (state: ReduxState) => {
    return {
      headers: state.tables.dataHeaders,
    };
  };
  return connect(mapStateToProps)(VoyagerView);
};

export default generatedVoyagerConnector;
