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

import ApiExplorer from './components/Services/ApiExplorer/ApiExplorer';
import VoyagerConnector from './components/Services/VoyagerView/VoyagerView';
import About from './components/Services/About/About';
import Login from './components/Login/Login';
import Settings from './components/Services/Settings/Container';
import MetadataOptions from './components/Services/Settings/MetadataOptions/MetadataOptions';
import MetadataStatus from './components/Services/Settings/MetadataStatus/MetadataStatus';
import AllowedQueries from './components/Services/Settings/AllowedQueries/AllowedQueries';
import Logout from './components/Services/Settings/Logout/Logout';

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

  const _dataRouterUtils = dataRouterUtils(store, composeOnEnterHooks);
  const requireSchema = _dataRouterUtils.requireSchema;
  const dataRouter = _dataRouterUtils.makeDataRouter;

  const _eventRouterUtils = eventRouterUtils(store, composeOnEnterHooks);
  const eventRouter = _eventRouterUtils.makeEventRouter;

  const remoteSchemaRouter = getRemoteSchemaRouter(store, composeOnEnterHooks);

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
    <Route path="/" component={App} onEnter={validateLogin(store)}>
      <Route path="login" component={Login} />
      <Route
        path=""
        component={Main}
        onEnter={composeOnEnterHooks([requireSchema, requireMigrationStatus])}
      >
        <Route path="">
          <IndexRoute component={ApiExplorer} />
          <Route path="api-explorer" component={ApiExplorer} />
          <Route path="voyager-view" component={VoyagerConnector} />
          <Route path="about" component={About} />
          <Route path="settings" component={Settings}>
            <IndexRedirect to="metadata-actions" />
            <Route path="metadata-actions" component={MetadataOptions} />
            <Route path="metadata-status" component={MetadataStatus} />
            <Route path="allowed-queries" component={AllowedQueries} />
            <Route path="logout" component={Logout} />
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
