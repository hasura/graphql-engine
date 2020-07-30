import React, { Component } from 'react';
import { Connect } from 'react-redux';
import { GraphQLVoyager } from 'graphql-voyager';

import Endpoints from '../../../Endpoints';
import ErrorBoundary from '../Common/ErrorBoundary';
import { Dispatch } from '../../../types';
import '../../../../node_modules/graphql-voyager/dist/voyager.css';
import './voyagerView.css';
import requestAction from '../../../utils/requestAction';

interface VoyagerViewProps {
  headers: Headers | Record<string, string>;
}

const mapStateToProps = (state: State) => {
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
// TODO: replace by redux State when it's defined
interface State {
  tables: {
    dataHeaders: Headers;
  };
}
type Props = VoyagerViewProps &
  ReturnType<typeof mapStateToProps> &
  ReturnType<typeof mapDispatchToProps>;

class VoyagerView extends Component<Props, State> {
  introspectionProvider = (query: string) =>
    this.props.requestAction(Endpoints.graphQLUrl, {
      method: 'POST',
      headers: this.props.headers,
      body: JSON.stringify({ query }),
    });

  loadWorker = async () => {
    const url = Endpoints.voyagerWorker;
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

  errorBoundaryTitle = 'Error in Voyager View';
  errorBoundaryMessage =
    'You might be seeing this because your schema is too large to be rendered.';

  render() {
    return (
      <ErrorBoundary
        title={this.errorBoundaryTitle}
        message={this.errorBoundaryMessage}
      >
        <GraphQLVoyager
          introspection={this.introspectionProvider}
          loadWorker={this.loadWorker}
        />
      </ErrorBoundary>
    );
  }
}

const generatedVoyagerConnector = (connect: Connect) => {
  return connect(mapStateToProps, mapDispatchToProps)(VoyagerView);
};

export default generatedVoyagerConnector;
