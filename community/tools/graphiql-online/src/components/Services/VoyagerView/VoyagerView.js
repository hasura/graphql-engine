import React, { Component } from 'react';
import { GraphQLVoyager } from 'graphql-voyager';
import {
  getGraphiQLHeadersFromLocalStorage,
} from '../ApiExplorer/ApiRequest/utils';
import { getHeadersAsJSON } from '../ApiExplorer/utils';
import fetch from 'isomorphic-fetch';
import '../../../../node_modules/graphql-voyager/dist/voyager.css';
import './voyagerView.css';

class VoyagerView extends Component {
  introspectionProvider(query) {
    const url = window.localStorage.getItem('ONLINE_GRAPHIQL_ENDPOINT');
    const HEADER_FROM_LS = getGraphiQLHeadersFromLocalStorage();
    let parsedHeader = {};
    if (HEADER_FROM_LS) {
      try {
        parsedHeader = JSON.parse(HEADER_FROM_LS);
      } catch (e) {
        parsedHeader = {};
        console.error(e);
      }
    }
    const headersFinal = getHeadersAsJSON(parsedHeader);
    return fetch(url, {
      method: 'POST',
      headers: headersFinal,
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
      headers: state.apiexplorer.displayedApi.request.headers,
    };
  };
  return connect(mapStateToProps)(VoyagerView);
};

export default generatedVoyagerConnector;
