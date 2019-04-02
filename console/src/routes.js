import React from 'react';
import { Route, IndexRoute } from 'react-router';

import { connect } from 'react-redux';

import { App, Main, PageNotFound } from 'components';

import { dataRouterUtils } from './components/Services/Data';

import { eventRouterUtils } from './components/Services/EventTrigger';

import { getCustomResolverRouter } from './components/Services/CustomResolver';

import { loadMigrationStatus } from './components/Main/Actions';

import { composeOnEnterHooks } from 'utils/router';

import generatedApiExplorer from './components/ApiExplorer/ApiExplorerGenerator';

import generatedLoginConnector from './components/Login/Login';

import metadataContainer from './components/Services/Data/Metadata/Container';
import metadataOptionsContainer from './components/Services/Data/Metadata/MetadataOptions';
import metadataStatusContainer from './components/Services/Data/Metadata/MetadataStatus';
import globals from './Globals';

import validateLogin from './components/Common/validateLogin';

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

  const _dataRouterUtils = dataRouterUtils(connect, store, composeOnEnterHooks);
  const requireSchema = _dataRouterUtils.requireSchema;
  const dataRouter = _dataRouterUtils.makeDataRouter;

  const _eventRouterUtils = eventRouterUtils(
    connect,
    store,
    composeOnEnterHooks
  );
  const eventRouter = _eventRouterUtils.makeEventRouter;

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
          <Route path="metadata" component={metadataContainer(connect)}>
            <Route path="status" component={metadataStatusContainer(connect)} />
            <Route
              path="options"
              component={metadataOptionsContainer(connect)}
            />
          </Route>
          {dataRouter}
          {eventRouter}
          {customResolverRouter}
        </Route>
      </Route>
      <Route path="404" component={PageNotFound} status="404" />
      <Route path="*" component={PageNotFound} status="404" />
    </Route>
  );
};

export default routes;
