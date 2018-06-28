import React from 'react';
import { Route, IndexRoute } from 'react-router';

import { connect } from 'react-redux';

import { App, Main, PageNotFound } from 'components';

import { dataRouter } from './components/Services/Data';

import { loadMigrationStatus } from './components/Main/Actions';

import { composeOnEnterHooks } from 'utils/router';

import generatedApiExplorer from './components/ApiExplorer/ApiExplorerGenerator';

const routes = store => {
  // load hasuractl migration status
  const requireMigrationStatus = (nextState, replaceState, cb) => {
    store.dispatch(loadMigrationStatus()).then(
      () => {
        cb();
      },
      () => {
        alert(
          'Not able to reach the cluster. Check if hasura console server is running or if cluster exists and try again'
        );
      }
    );
    return;
  };

  // loads schema
  const dataRouterUtils = dataRouter(connect, store, composeOnEnterHooks);
  const requireSchema = dataRouterUtils.requireSchema;
  const makeDataRouter = dataRouterUtils.makeDataRouter;

  return (
    <Route path="/" component={App}>
      <Route
        path=""
        component={Main}
        onEnter={composeOnEnterHooks([requireMigrationStatus])}
      >
        <Route path="">
          <IndexRoute
            onEnter={requireSchema}
            component={generatedApiExplorer(connect)}
          />
          <Route
            path="api-explorer"
            component={generatedApiExplorer(connect)}
            onEnter={requireSchema}
          />
          {makeDataRouter}
        </Route>
      </Route>
      <Route path="404" component={PageNotFound} status="404" />
      <Route path="*" component={PageNotFound} status="404" />
    </Route>
  );
};

export default routes;
