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

  loadWorker = async () => {
    const url =
      'https://cdn.jsdelivr.net/npm/graphql-voyager@1.0.0-rc.30/dist/voyager.worker.min.js';
    const response = await fetch(url);
    const payload = await response.text();
    // HACK: to increase viz.js memory size from 16mb to 512mb
    // NOTE: this is the same hack that is used here too: https://github.com/APIs-guru/graphql-voyager/blob/v1.0.0-rc.29/src/utils/index.ts#L21
    const newPayload = payload
      .replace('||16777216;', '||(16777216 * 16 * 2);')
      .replace('||5242880;', '||(5242880 * 16 * 2);');
    const script = new Blob([newPayload], { type: 'application/javascript' });
    const workerURL = URL.createObjectURL(script);
    return new Worker(workerURL);
  };

  render() {
    return (
      <VoyagerViewErrorBoundary>
        <GraphQLVoyager
          introspection={this.introspectionProvider}
          loadWorker={this.loadWorker}
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
