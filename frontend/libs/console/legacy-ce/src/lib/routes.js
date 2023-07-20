import React from 'react';
import { Route, IndexRedirect, IndexRoute } from 'react-router';
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
import { eventsRoutes } from './components/Services/Events';
import generatedApiExplorer from './components/Services/ApiExplorer/ApiExplorer';
import generatedVoyagerConnector from './components/Services/VoyagerView/VoyagerView';
import generatedLoginConnector from './components/Login/Login';
import settingsContainer from './components/Services/Settings/Container';
import ApiContainer from './components/Services/ApiExplorer/Container';
import metadataOptionsConnector from './components/Services/Settings/MetadataOptions/MetadataOptions';
import metadataStatusConnector from './components/Services/Settings/MetadataStatus/MetadataStatus';
import inheritedRolesConnector from './components/Services/Settings/InheritedRoles/InheritedRoles';
import logoutConnector from './components/Services/Settings/Logout/Logout';
import aboutConnector from './components/Services/Settings/About/About';
import { showErrorNotification } from './components/Services/Common/Notification';
import { CLI_CONSOLE_MODE } from './constants';
import { SupportContainer } from './components/Services/Support/SupportContainer';
import HelpPage from './components/Services/Support/HelpPage';
import FormRestView from './components/Services/ApiExplorer/Rest/Form';
import { HerokuCallbackHandler } from './components/Services/Data/DataSources/CreateDataSource/Heroku/TempCallback';
import { NeonCallbackHandler } from './components/Services/Data/DataSources/CreateDataSource/Neon/TempCallback';
import { SlackCallbackHandler } from './features/SchemaRegistry/components/TempSlackCallback';
import InsecureDomains from './components/Services/Settings/InsercureDomains/AllowInsecureDomains';
import AuthContainer from './components/Services/Auth/AuthContainer';
import { FeatureFlags } from './features/FeatureFlags';
import { AllowListDetail } from './components/Services/AllowList';
import { RestEndpointList } from './features/RestEndpoints/components/RestEndpointList';
import { RestEndpointDetailsPage } from './features/RestEndpoints/components/RestEndpointDetails/RestEndpointDetailsPage';

const routes = store => {
  // load hasuraCliServer migration status
  const requireMigrationStatus = (nextState, replaceState, cb) => {
    const { dispatch } = store;

    if (globals.consoleMode === CLI_CONSOLE_MODE) {
      dispatch(loadMigrationStatus()).then(
        () => {
          cb();
        },
        r => {
          if (r && r.code === 'data_api_error') {
            dispatch(showErrorNotification('Error', null, r));
          } else {
            dispatch(
              showErrorNotification(
                'Connection error',
                'Hasura console is not able to reach your Hasura CLI instance. Please ensure that your ' +
                  'instance is running and the endpoint is configured correctly.'
              )
            );
          }
          cb();
        }
      );
    } else {
      cb();
    }

    return;
  };

  const _dataRouterUtils = dataRouterUtils(
    connect,
    store,
    composeOnEnterHooks,
    () => {
      return {};
    },
    () => {
      return { public: [], personal: [] };
    }
  );
  const requireSource = _dataRouterUtils.requireSource;
  const dataRouter = _dataRouterUtils.makeDataRouter;

  const remoteSchemaRouter = getRemoteSchemaRouter(
    connect,
    store,
    composeOnEnterHooks
  );

  const actionsRouter = getActionsRouter(connect, store, composeOnEnterHooks);

  return (
    <Route
      path="/"
      component={App}
      onEnter={composeOnEnterHooks([validateLogin(store)])}
    >
      {/*Temp route, it'll be in dashboard*/}
      <Route path="login" component={generatedLoginConnector(connect)} />
      <Route path="heroku-callback" component={HerokuCallbackHandler} />
      <Route path="neon-integration/callback" component={NeonCallbackHandler} />
      <Route
        path="slack-integration/callback"
        component={SlackCallbackHandler}
      />
      <Route path="" component={AuthContainer}>
        <Route
          path=""
          component={Main}
          onEnter={composeOnEnterHooks([
            requireSource,
            requireMigrationStatus,
            requireAsyncGlobals(store),
          ])}
        >
          <IndexRoute component={ApiContainer} />
          <Route path="api" component={ApiContainer}>
            <IndexRedirect to="api-explorer" />
            <Route
              path="api-explorer"
              component={generatedApiExplorer(connect)}
            />
            <Route path="rest">
              <IndexRedirect to="list" />
              <Route path="create" component={FormRestView} />
              <Route path="list" component={RestEndpointList} />
              <Route path="details/:name" component={RestEndpointDetailsPage} />
              <Route path="edit/:name" component={FormRestView} />
            </Route>
            <Route path="allow-list">
              <IndexRedirect to="detail" />
              <Route
                path="detail(/:name)(/:section)"
                component={AllowListDetail}
              />
            </Route>
          </Route>

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
            <Route path="logout" component={logoutConnector(connect)} />
            <Route path="about" component={aboutConnector(connect)} />
            <Route path="inherited-roles" component={inheritedRolesConnector} />

            <Route path="insecure-domain" component={InsecureDomains} />

            <Route path="feature-flags" component={FeatureFlags} />
          </Route>
          {dataRouter}
          {remoteSchemaRouter}
          {actionsRouter}
          {eventsRoutes}
          <Route path="support" component={SupportContainer}>
            <Route path="forums" component={HelpPage} />
          </Route>
        </Route>
      </Route>

      <Route path="404" component={PageNotFound} status="404" />
      <Route path="*" component={PageNotFound} status="404" />
    </Route>
  );
};

export default routes;
