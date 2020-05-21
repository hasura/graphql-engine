import React, { Component } from 'react';
import { Connect } from 'react-redux';
import { GraphQLVoyager } from 'graphql-voyager';
import fetch from 'isomorphic-fetch';

import Endpoints from '../../../Endpoints';
import '../../../../node_modules/graphql-voyager/dist/voyager.css';
import './voyagerView.css';

interface VoyagerViewProps {
  headers: Headers;
}

interface StateProps {
  headers: Headers;
}

// TODO: replace by redux State when it's defined
interface State {
  tables: {
    dataHeaders: Headers;
  };
}

type Props = VoyagerViewProps & StateProps;

class VoyagerView extends Component<Props, State> {
  introspectionProvider = (query: string) => {
    return fetch(Endpoints.graphQLUrl, {
      method: 'POST',
      headers: this.props.headers,
      body: JSON.stringify({ query }),
    }).then(response => response.json());
  };

  render() {
    return (
      <GraphQLVoyager
        introspection={this.introspectionProvider}
        workerURI="https://cdn.jsdelivr.net/npm/graphql-voyager@1.0.0-rc.27/dist/voyager.worker.min.js"
      />
    );
  }
}

const generatedVoyagerConnector = (connect: Connect) => {
  const mapStateToProps = (state: State) => {
    return {
      headers: state.tables.dataHeaders,
    };
  };
  return connect(mapStateToProps)(VoyagerView);
};

export default generatedVoyagerConnector;
