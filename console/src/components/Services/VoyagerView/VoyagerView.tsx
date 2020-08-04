import React from 'react';
import { connect } from 'react-redux';
import { GraphQLVoyager } from 'graphql-voyager';

import Endpoints from '../../../Endpoints';
import ErrorBoundary from '../Common/ErrorBoundary';
import { Dispatch, ReduxState } from '../../../types';
import '../../../../node_modules/graphql-voyager/dist/voyager.css';
import './voyagerView.css';
import requestAction from '../../../utils/requestAction';

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

interface VoyagerViewProps
  extends ReturnType<typeof mapStateToProps>,
    ReturnType<typeof mapDispatchToProps> {}

const VoyagerView = (props: VoyagerViewProps) => {
  const introspectionProvider = (query: string) =>
    props.requestAction(Endpoints.graphQLUrl, {
      method: 'POST',
      headers: props.headers,
      body: JSON.stringify({ query }),
    });

  const loadWorker = async () => {
    const url = Endpoints.voyagerWorker;
    const response = await fetch(url);
    const payload = await response.text();
    // HACK: to increase viz.js memory size from 16mb to 128mb
    // https://github.com/APIs-guru/graphql-voyager/blob/v1.0.0-rc.29/src/utils/index.ts#L21
    const newPayload = payload
      .replace('||16777216;', '||(16777216 * 8);')
      .replace('||5242880;', '||(5242880 * 8);');
    const script = new Blob([newPayload], { type: 'application/javascript' });
    const workerURL = URL.createObjectURL(script);
    return new Worker(workerURL);
  };

  return (
    <ErrorBoundary
      message="You might be seeing this because your schema is too large to be handled by Voyager."
      helmetTitle="Voyager Error | Hasura"
    >
      <GraphQLVoyager
        introspection={introspectionProvider}
        loadWorker={loadWorker}
      />
    </ErrorBoundary>
  );
};

const ConnectedVoyagerView = connect(
  mapStateToProps,
  mapDispatchToProps
)(VoyagerView);

export default ConnectedVoyagerView;
