import React from 'react';
import { Route, IndexRoute } from 'react-router';

import { connect } from 'react-redux';

import { App, Main, PageNotFound } from 'components';

import { dataRouter } from './components/Services/Data';

import { eventRouter } from './components/Services/EventTrigger';

import { loadMigrationStatus } from './components/Main/Actions';

import { composeOnEnterHooks } from 'utils/router';

import generatedApiExplorer from './components/ApiExplorer/ApiExplorerGenerator';

import generatedLoginConnector from './components/Login/Login';

import { metadataConnector } from './components/Services/Data';

import globals from './Globals';

import validateLogin from './components/Common/validateLogin';

import { getCustomResolverRouter } from './components/Services/CustomResolver';

const routes = store => {
  // load hasuractl migration status
  const requireMigrationStatus = (nextState, replaceState, cb) => {
    if (globals.consoleMode === 'cli') {
      store.dispatch(loadMigrationStatus()).then(
        () => {
          cb();
        },
        r => {
          if (r.code === 'data_api_error') {
            if (globals.adminSecret) {
              alert('Hasura CLI: ' + r.message);
            } else {
              alert(
                `Looks like CLI is not configured with the ${
                  globals.adminSecretLabel
                }. Please configure and try again`
              );
            }
          } else {
            alert(
              'Hasura console is not able to reach your Hasura GraphQL engine instance. Please ensure that your ' +
              'instance is running and the endpoint is configured correctly.'
            );
          }
        }
      );
    } else {
      cb();
    }
    return;
  };

  // loads schema
  const dataRouterUtils = dataRouter(connect, store, composeOnEnterHooks);
  const eventRouterUtils = eventRouter(connect, store, composeOnEnterHooks);
  const requireSchema = dataRouterUtils.requireSchema;
  const makeDataRouter = dataRouterUtils.makeDataRouter;
  const makeEventRouter = eventRouterUtils.makeEventRouter;

  const customResolverRouter = getCustomResolverRouter(
    connect,
    store,
    composeOnEnterHooks
  );
  return (
    <Route path="/" component={App} onEnter={validateLogin(store)}>
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
          <Route path="metadata" component={metadataConnector(connect)} />
          {makeDataRouter}
          {makeEventRouter}
          {customResolverRouter}
        </Route>
      </Route>
      <Route path="404" component={PageNotFound} status="404" />
      <Route path="*" component={PageNotFound} status="404" />
    </Route>
  );
};

export default routes;
