import React, { Component } from 'react';
import { Connect } from 'react-redux';
import { GraphQLVoyager } from 'graphql-voyager';
import 'graphql-voyager/dist/voyager.css';

import { Analytics, REDACT_EVERYTHING } from '../../../features/Analytics';

import Endpoints from '../../../Endpoints';
import './voyagerView.css';
import requestAction from '../../../utils/requestAction';
import { Dispatch, ReduxState } from '../../../types';

interface VoyagerViewProps {
  headers: Headers;
}

const mapStateToProps = (state: ReduxState) => {
  return {
    headers: state.tables.dataHeaders,
  };
};
const mapDispatchToProps = (dispatch: Dispatch) => {
  return {
    requestAction: (url: string, options: RequestInit) =>
      dispatch(requestAction(url, options)),
  };
};

type Props = VoyagerViewProps &
  ReturnType<typeof mapStateToProps> &
  ReturnType<typeof mapDispatchToProps>;

class VoyagerView extends Component<Props, ReduxState> {
  introspectionProvider = (query: string) =>
    this.props.requestAction(Endpoints.graphQLUrl, {
      method: 'POST',
      headers: this.props.headers,
      body: JSON.stringify({ query }),
    });

  override render() {
    return (
      <Analytics name="VoyagerView" {...REDACT_EVERYTHING}>
        <div>
          <GraphQLVoyager
            introspection={this.introspectionProvider}
            workerURI="https://cdn.jsdelivr.net/npm/graphql-voyager@1.0.0-rc.27/dist/voyager.worker.min.js"
          />
        </div>
      </Analytics>
    );
  }
}

const generatedVoyagerConnector = (connect: Connect) => {
  return connect(mapStateToProps, mapDispatchToProps)(VoyagerView);
};

export default generatedVoyagerConnector;
