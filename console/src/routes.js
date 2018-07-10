import React from 'react';
import { Route, IndexRoute } from 'react-router';

import { connect } from 'react-redux';

import { App, Main, PageNotFound } from 'components';

import { dataRouter } from './components/Services/Data';

import { loadMigrationStatus } from './components/Main/Actions';

import { composeOnEnterHooks } from 'utils/router';

import generatedApiExplorer from './components/ApiExplorer/ApiExplorerGenerator';

import generatedLoginConnector from './components/Login/Login';

import globals from './Globals';

const routes = store => {
  // load hasuractl migration status
  const requireMigrationStatus = (nextState, replaceState, cb) => {
    if (globals.consoleMode === 'cli') {
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
    } else {
      cb();
    }
    return;
  };

  // loads schema
  const dataRouterUtils = dataRouter(connect, store, composeOnEnterHooks);
  const requireSchema = dataRouterUtils.requireSchema;
  const makeDataRouter = dataRouterUtils.makeDataRouter;

  return (
    <Route path="/" component={App}>
      <Route path="login" component={generatedLoginConnector(connect)} />
      <Route
        path=""
        component={Main}
        onEnter={composeOnEnterHooks([requireSchema, requireMigrationStatus])}
      >
        <Route path="">
          <IndexRoute component={generatedApiExplorer(connect)} />
          <Route
            path="api-explorer"
            component={generatedApiExplorer(connect)}
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
