import React, { Component } from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { GraphQLVoyager } from 'graphql-voyager';
import fetch from 'isomorphic-fetch';

import Endpoints from '../../../Endpoints';
import '../../../../node_modules/graphql-voyager/dist/voyager.css';
import './voyagerView.css';
import requestAction from '../../../utils/requestAction';

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
const mapState = (state: State) => {
  return {
    headers: state.tables.dataHeaders,
  };
};
const mapDispatch = {
  requestAction,
};

const connector = connect(mapState, mapDispatch);
type PropsFromRedux = ConnectedProps<typeof connector>;

type Props = PropsFromRedux & VoyagerViewProps & StateProps;

class VoyagerView extends Component<Props, State> {
  introspectionProvider = (query: string) => {
    return this.props.requestAction(Endpoints.graphQLUrl, {
      method: 'POST',
      headers: this.props.headers,
      body: JSON.stringify({ query }),
    });
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

export default connector(VoyagerView);
