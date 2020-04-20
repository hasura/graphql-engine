import React from 'react';
import { Route, IndexRoute, IndexRedirect } from 'react-router';

import { connect } from 'react-redux';

import { App, Main, PageNotFound } from 'components';

import globals from './Globals';

import validateLogin from './utils/validateLogin';

import { composeOnEnterHooks } from 'utils/router';

import { loadMigrationStatus } from './components/Main/Actions';

import { dataRouterUtils } from './components/Services/Data';

import { eventRouterUtils } from './components/Services/EventTrigger';

import { getRemoteSchemaRouter } from './components/Services/RemoteSchema';

import { getActionsRouter } from './components/Services/Actions';

import generatedApiExplorer from './components/Services/ApiExplorer/ApiExplorer';

import generatedVoyagerConnector from './components/Services/VoyagerView/VoyagerView';

import about from './components/Services/About/About';

import generatedLoginConnector from './components/Login/Login';

import settingsContainer from './components/Services/Settings/Container';
import metadataOptionsContainer from './components/Services/Settings/MetadataOptions/MetadataOptions';
import metadataStatusContainer from './components/Services/Settings/MetadataStatus/MetadataStatus';
import allowedQueriesContainer from './components/Services/Settings/AllowedQueries/AllowedQueries';
import logoutContainer from './components/Services/Settings/Logout/Logout';

import { showErrorNotification } from './components/Services/Common/Notification';
import { CLI_CONSOLE_MODE } from './constants';
import UIKit from './components/UIKit/';
import { Heading } from './components/UIKit/atoms';

const routes = store => {
  // load hasuractl migration status
  const requireMigrationStatus = (nextState, replaceState, cb) => {
    const { dispatch } = store;

    if (globals.consoleMode === CLI_CONSOLE_MODE) {
      dispatch(loadMigrationStatus()).then(
        () => {
          cb();
        },
        r => {
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

  const _eventRouterUtils = eventRouterUtils(
    connect,
    store,
    composeOnEnterHooks
  );
  const eventRouter = _eventRouterUtils.makeEventRouter;

  const remoteSchemaRouter = getRemoteSchemaRouter(
    connect,
    store,
    composeOnEnterHooks
  );

  const actionsRouter = getActionsRouter(connect, store, composeOnEnterHooks);

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
        requireMigrationStatus,
      ])}
    >
      <Route path="login" component={generatedLoginConnector(connect)} />
      <Route path="" component={Main}>
        <Route path="">
          <IndexRoute
            component={generatedApiExplorer(connect)}
            onEnter={composeOnEnterHooks([requireSchema])}
          />
          <Route
            path="api-explorer"
            component={generatedApiExplorer(connect)}
            onEnter={composeOnEnterHooks([requireSchema])}
          />
          <Route
            path="voyager-view"
            component={generatedVoyagerConnector(connect)}
          />
          <Route path="about" component={about(connect)} />
          <Route path="settings" component={settingsContainer(connect)}>
            <IndexRedirect to="metadata-actions" />
            <Route
              path="metadata-actions"
              component={metadataOptionsContainer(connect)}
            />
            <Route
              path="metadata-status"
              component={metadataStatusContainer(connect)}
            />
            <Route
              path="allowed-queries"
              component={allowedQueriesContainer(connect)}
            />
            <Route path="logout" component={logoutContainer(connect)} />
          </Route>
          {dataRouter}
          {eventRouter}
          {remoteSchemaRouter}
          {actionsRouter}
          {uiKitRouter}
        </Route>
      </Route>
      <Route path="404" component={PageNotFound} status="404" />
      <Route path="*" component={PageNotFound} status="404" />
    </Route>
  );
};

export default routes;
