import React from 'react';
import { Route, IndexRoute, IndexRedirect, EnterHook } from 'react-router';

import { connect } from 'react-redux';

import globals from './Globals';

import { App, Main, PageNotFound } from './components';

import validateLogin from './utils/validateLogin';

import { requireAsyncGlobals } from './components/App/Actions';

import { composeOnEnterHooks } from './utils/router';

import { loadMigrationStatus } from './components/Main/Actions';

import { dataRouterUtils } from './components/Services/Data';

import { getRemoteSchemaRouter } from './components/Services/RemoteSchema';

import { getActionsRouter } from './components/Services/Actions';

import { getEventsRouter } from './components/Services/Events';

import generatedApiExplorer from './components/Services/ApiExplorer/ApiExplorer';

import generatedVoyagerConnector from './components/Services/VoyagerView/VoyagerView';

import generatedLoginConnector from './components/Login/Login';

import settingsContainer from './components/Services/Settings/Container';
import metadataOptionsConnector from './components/Services/Settings/MetadataOptions/MetadataOptions';
import metadataStatusConnector from './components/Services/Settings/MetadataStatus/MetadataStatus';
import allowedQueriesConnector from './components/Services/Settings/AllowedQueries/AllowedQueries';
import logoutConnector from './components/Services/Settings/Logout/Logout';
import aboutConnector from './components/Services/Settings/About/About';

import { showErrorNotification } from './components/Services/Common/Notification';
import { CLI_CONSOLE_MODE } from './constants';
import UIKit from './components/UIKit';
import { Heading } from './components/UIKit/atoms';
import { ReduxState, ReplaceRouterState } from './types';


const routes = (store: any) => {
  // load hasuractl migration status
  const requireMigrationStatus = (nextState: unknown, replaceState: unknown, cb: any) => {
    const { dispatch } = store;

    if (globals.consoleMode === CLI_CONSOLE_MODE) {
      dispatch(loadMigrationStatus()).then(
        () => {
          cb();
        },
        (r: Record<string, any>) => {
          if (r.code === 'data_api_error') {
            dispatch(showErrorNotification('Error', null, r));
          } else {
            dispatch(
              showErrorNotification(
                'Connection error',
                'Hasura console is not able to reach your Hasura GraphQL engine instance. Please ensure that your ' +
                  'instance is running and the endpoint is configured correctly.'
              )
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

  const remoteSchemaRouter = getRemoteSchemaRouter(
    connect,
    store,
    composeOnEnterHooks
  );

  const actionsRouter = getActionsRouter(connect, store, composeOnEnterHooks);

  const eventsRouter = getEventsRouter(connect, store, composeOnEnterHooks);

  const uiKitRouter = globals.isProduction ? null : (
    <Route
      path="/ui-elements"
      // TODO: fix me
      component={() => (
        <div>
          <Heading />
          <UIKit />
        </div>
      )}
    />
  );

  return (
    <Route
      path="/"
      component={App}
      onEnter={composeOnEnterHooks([
        validateLogin(store),
        requireAsyncGlobals(store),
      ]) as EnterHook}
    >
      <Route path="login" component={generatedLoginConnector(connect)} />
      <Route
        path=""
        component={Main}
        onEnter={composeOnEnterHooks([requireSchema, requireMigrationStatus]) as EnterHook}
      >
        <Route path="">
          <IndexRoute component={generatedApiExplorer(connect)} />
          <Route
            path="api-explorer"
            component={generatedApiExplorer(connect)}
          />
          <Route
            path="voyager-view"
            component={generatedVoyagerConnector(connect)}
          />
          <Route path="settings" component={settingsContainer(connect)}>
            <IndexRedirect to="metadata-actions" />
            <Route
              path="metadata-actions"
              component={metadataOptionsConnector(connect)}
            />
            <Route
              path="metadata-status"
              component={metadataStatusConnector(connect)}
            />
            <Route
              path="allowed-queries"
              component={allowedQueriesConnector(connect)}
            />
            <Route path="logout" component={logoutConnector(connect)} />
            <Route path="about" component={aboutConnector(connect)} />
          </Route>
          {dataRouter}
          {remoteSchemaRouter}
          {actionsRouter}
          {eventsRouter}
          {uiKitRouter}
        </Route>
      </Route>
      <Route path="404" component={PageNotFound} />
      <Route path="*" component={PageNotFound} />
    </Route>
  );
};

export default routes;
