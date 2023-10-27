import React from 'react';
import { Route, IndexRoute, IndexRedirect } from 'react-router';
import { connect } from 'react-redux';

import { Main } from './components';
import { OAUTH_CALLBACK_URL } from './constants';
import globals from './Globals';
import { relativeModulePath } from './components/Services/Metrics/constants';
import {
  loadInconsistentObjects,
  redirectToMetadataStatus,
  isMetadataStatusPage,
  prefetchSurveysData,
  prefetchOnboardingData,
  prefetchEELicenseInfo,
  PageNotFound,
  dataHeaders,
  loadAdminSecretState,
} from '@hasura/console-legacy-ce';
import {
  dataRouterUtils,
  eventsRoutes,
  getActionsRouter,
  getRemoteSchemaRouter,
  generatedApiExplorer,
  generatedVoyagerConnector,
} from '@hasura/console-legacy-ce';

import { requireAsyncGlobals, App } from '@hasura/console-legacy-ce';

import {
  loadMigrationStatus,
  metadataContainer,
  metadataOptionsContainer,
  metadataStatusContainer,
  logoutContainer,
  aboutContainer,
  ApiContainer,
  CreateRestView,
  RestEndpointList,
  InheritedRolesContainer,
  ApiLimits,
  IntrospectionOptions,
  InsecureDomains,
  FeatureFlags,
  isMonitoringTabSupportedEnvironment,
  AllowListDetail,
  PrometheusSettings,
  QueryResponseCaching,
  OpenTelemetryFeature,
  MultipleAdminSecretsPage,
  MultipleJWTSecretsPage,
  SingleSignOnPage,
  SchemaRegistryContainer,
  RestEndpointDetailsPage,
} from '@hasura/console-legacy-ce';

import AccessDeniedComponent from './components/AccessDenied/AccessDenied';
import { restrictedPathsMetadata } from './utils/redirectUtils';
import generatedCallbackConnector from './components/OAuthCallback/OAuthCallback';
import generatedLoginConnector from './components/Login/Login';
import validateLogin from './utils/validateLogin';
import { composeOnEnterHooks } from './utils/router';
import { decodeToken, checkAccess } from './utils/computeAccess';
import preLoginHook from './utils/preLoginHook';
import metricsRouter from './components/Services/Metrics/MetricsRouter';
import { notifyRouteChangeToAppcues } from './utils/appCues';
import extendedGlobals from './Globals';

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
                `Looks like CLI is not configured with the ${globals.adminSecretLabel}. Please configure and try again`
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

  const getSchemaAccess = () => {
    const mainData = store.getState().main;
    const { oAuthResponse } = mainData;
    if ('id_token' in oAuthResponse) {
      const decoded = decodeToken(oAuthResponse.id_token);
      if (!decoded) {
        this.invalidateToken();
      }
      const { allowed_schemas } = decoded.payload;
      return { allowedSchemas: allowed_schemas } || {};
    }
    return {};
  };

  const getTableAccess = () => {
    const mainData = store.getState().main;
    const { oAuthResponse } = mainData;
    if ('id_token' in oAuthResponse) {
      const decoded = decodeToken(oAuthResponse.id_token);
      if (!decoded) {
        this.invalidateToken();
      }
      const { allowed_tables: allowedTables } = decoded.payload;
      return allowedTables;
    }
    return {};
  };

  // TODO: need to be parsed as per the claims
  // const allowedSchemas = ['public'];
  const _dataRouterUtils = dataRouterUtils(
    connect,
    store,
    composeOnEnterHooks,
    getSchemaAccess,
    getTableAccess
  );
  const requireSource = _dataRouterUtils.requireSource;
  const dataRouter = _dataRouterUtils.makeDataRouter;

  const actionsRouter = getActionsRouter(connect, store, composeOnEnterHooks);

  const remoteSchemaRouter = getRemoteSchemaRouter(
    connect,
    store,
    composeOnEnterHooks
  );

  const loadInconsistentMetaDataObjects = (nextState, replaceState, cb) => {
    const { dispatch } = store;
    dispatch(loadInconsistentObjects({ shouldReloadMetadata: false })).then(
      () => {
        const { metadata } = store.getState();
        if (
          metadata.inconsistentObjects.length > 0 ||
          metadata.inconsistentInheritedRoles?.length > 0
        ) {
          if (!isMetadataStatusPage()) {
            dispatch(redirectToMetadataStatus());
          }
        }
        cb();
      }
    );
  };

  const accessDeniedRedirects = (accessState, nextState, replaceState, cb) => {
    for (let i = Object.keys(restrictedPathsMetadata).length - 1; i >= 0; i--) {
      const restrictedPath = Object.keys(restrictedPathsMetadata)[i];
      const restrictedPathData = restrictedPathsMetadata[restrictedPath];
      if (nextState.location.pathname.indexOf(restrictedPath) === 0) {
        if (!accessState[restrictedPathData.keyInAccessState]) {
          replaceState(restrictedPathData.replace);
          cb();
          break;
        }
      }
    }
  };

  const validateAccessToRoute = (nextState, replaceState, cb) => {
    const mainData = store.getState().main;
    const { oAuthResponse } = mainData;
    let accessState = {};
    if ('id_token' in oAuthResponse) {
      const decoded = decodeToken(oAuthResponse.id_token);
      if (!decoded) {
        this.invalidateToken();
      }
      // console.log(decoded);
      const { payload } = decoded;
      const { collaborator_privileges } = payload;

      const accessObj = checkAccess(collaborator_privileges);
      // console.log(accessObj);
      accessState = { ...accessObj };
      /* Handle only metric scenario */

      accessDeniedRedirects(accessState, nextState, replaceState, cb);

      if (
        'hasMetricAccess' in accessState &&
        accessState.hasMetricAccess &&
        !('hasDataAccess' in accessState && accessState.hasDataAccess) &&
        !('hasGraphQLAccess' in accessState && accessState.hasGraphQLAccess)
      ) {
        if (
          nextState.location.pathname.indexOf(relativeModulePath) === -1 &&
          nextState.location.pathname.indexOf('/access-denied') === -1
        ) {
          replaceState(relativeModulePath);
        }
        cb();
        return;
      }
    }
    cb();
  };

  const shouldLoadAsyncGlobals = storeLocal => {
    let shouldLoadServer = true;
    let shouldLoadOpts = true;
    const mainData = storeLocal.getState().main;
    const { oAuthResponse } = mainData;
    let accessState = {};
    if ('id_token' in oAuthResponse) {
      const decoded = decodeToken(oAuthResponse.id_token);
      if (!decoded) {
        this.invalidateToken();
      }
      // console.log(decoded);
      const { payload } = decoded;
      const { collaborator_privileges } = payload;

      const accessObj = checkAccess(collaborator_privileges);
      // console.log(accessObj);
      accessState = { ...accessObj };
      /* Handle only metric scenario */

      if ('hasDataAccess' in accessState && accessState.hasDataAccess) {
        shouldLoadOpts = true;
      } else {
        shouldLoadOpts = false;
      }
      if ('hasGraphQLAccess' in accessState && accessState.hasGraphQLAccess) {
        shouldLoadServer = true;
      } else {
        shouldLoadServer = false;
      }
    }
    return { shouldLoadOpts, shouldLoadServer };
  };

  const generateOnEnterHooks = (...args) => {
    if (
      !!globals.hasuraCloudTenantId &&
      globals.consoleType === 'cloud' &&
      globals.userRole === 'owner'
    ) {
      prefetchSurveysData();
      prefetchOnboardingData();
    }

    if (globals.consoleType === 'pro-lite') {
      prefetchEELicenseInfo(dataHeaders(store.getState));
    }

    const onEnterHooks = [validateAccessToRoute];
    const { shouldLoadOpts, shouldLoadServer } = shouldLoadAsyncGlobals(store);
    if (shouldLoadOpts || shouldLoadServer) {
      onEnterHooks.push(
        requireAsyncGlobals(store, shouldLoadOpts, shouldLoadServer)
      );
      if (shouldLoadOpts) {
        onEnterHooks.push(requireSource);
      }
    }
    return composeOnEnterHooks(onEnterHooks)(...args);
  };

  /**
   * ## checkIfAdmin
   * This function checks if the user is an admin or not and redirects to the access denied page if not.
   * This is used to hide the security tab from non-admin users.
   */
  const checkIfAdmin = (nextState, replaceState) => {
    const mainData = store.getState().main;
    // when console type is pro-lite only admin secret login is allowed, making this check unnecessary
    // ie. admin privileges are already checked in the login process
    if (globals.consoleType === 'pro-lite') return; // show security tab

    // when consoleType === pro and if admin secret is provided, show security tab
    if (
      globals.consoleType === 'pro' &&
      (extendedGlobals.adminSecret ||
        loadAdminSecretState() ||
        globals.adminSecret) &&
      (extendedGlobals.adminSecret ||
        loadAdminSecretState() ||
        globals.adminSecret) !== ''
    )
      return;

    // cloud cli doesn't have any privileges when `hasura console` command is executed, it will only have previleges when `hasura pro console` is executed.
    // this will make sure that security tab is visible even when the users are running `hasura console` command with valid admin secret
    if (globals.consoleType === 'cloud' && globals.consoleMode === 'cli') {
      // this will be true when `hasura console` is executed with a valid admin secret -> through which security tab APIs are accessible
      if (globals.adminSecret && globals.adminSecret !== '') return; // show security tab
    }

    // check privileges for all other cases
    if (!mainData.project.privileges.includes('admin')) {
      replaceState('api/security/access_denied'); // show access denied page
    }
  };

  return (
    <Route
      path="/"
      component={App}
      onEnter={validateLogin(store)}
      onChange={notifyRouteChangeToAppcues}
    >
      <Route
        path="login"
        onEnter={preLoginHook}
        component={generatedLoginConnector(connect)}
      />
      <Route
        path={OAUTH_CALLBACK_URL}
        component={generatedCallbackConnector(connect)}
      />
      <Route path="" component={Main} onEnter={generateOnEnterHooks}>
        <IndexRoute component={ApiContainer} />
        <Route path="api" component={ApiContainer}>
          <IndexRedirect to="api-explorer" />
          <Route
            path="api-explorer"
            component={generatedApiExplorer(connect)}
          />
          <Route path="rest">
            <IndexRedirect to="list" />
            <Route path="create" component={CreateRestView} />
            <Route path="list" component={RestEndpointList} />
            <Route path="details/:name" component={RestEndpointDetailsPage} />
            <Route path="edit/:name" component={CreateRestView} />
          </Route>
          <Route path="schema-registry" component={SchemaRegistryContainer} />
          <Route
            path="schema-registry/:id"
            component={SchemaRegistryContainer}
          />
          <Route path="allow-list">
            <IndexRedirect to="detail" />
            <Route
              path="detail(/:name)(/:section)"
              component={AllowListDetail}
            />
          </Route>
          <Route path="" onEnter={composeOnEnterHooks([checkIfAdmin])}>
            <Route path="security" component={ApiLimits} />
            <Route path="security/api_limits" component={ApiLimits} />
            <Route
              path="security/introspection"
              component={IntrospectionOptions}
            />
          </Route>
          <Route
            path="security/access_denied"
            component={AccessDeniedComponent}
          />
        </Route>
        <Route
          path="voyager-view"
          component={generatedVoyagerConnector(connect)}
        />
        <Route path="access-denied" component={AccessDeniedComponent} />
        {/* Disable monitoring tab when consoleType is pro-lite or oss */}
        {isMonitoringTabSupportedEnvironment(globals) && metricsRouter(connect)}
        <Route
          path=""
          onEnter={composeOnEnterHooks([
            validateAccessToRoute,
            requireSource,
            requireMigrationStatus,
            loadInconsistentMetaDataObjects,
          ])}
        >
          <Route path="settings" component={metadataContainer(connect)}>
            <IndexRedirect to="metadata-actions" />
            <Route path="schema-registry" component={SchemaRegistryContainer} />
            <Route
              path="schema-registry/:id"
              component={SchemaRegistryContainer}
            />
            <Route
              path="metadata-actions"
              component={metadataOptionsContainer(connect)}
            />
            <Route
              path="metadata-status"
              component={metadataStatusContainer(connect)}
            />
            <Route path="logout" component={logoutContainer(connect)} />
            <Route path="about" component={aboutContainer(connect)} />
            <Route path="inherited-roles" component={InheritedRolesContainer} />
            <Route path="insecure-domain" component={InsecureDomains} />
            <Route path="prometheus-settings" component={PrometheusSettings} />
            <Route
              path="query-response-caching"
              component={QueryResponseCaching}
            />
            <Route
              path="multiple-admin-secrets"
              component={MultipleAdminSecretsPage}
            />
            <Route
              path="multiple-jwt-secrets"
              component={MultipleJWTSecretsPage}
            />
            <Route path="single-sign-on" component={SingleSignOnPage} />
            <Route path="opentelemetry" component={OpenTelemetryFeature} />
            <Route path="feature-flags" component={FeatureFlags} />
          </Route>
          {dataRouter}
          {actionsRouter}
          {eventsRoutes}
          {remoteSchemaRouter}
        </Route>
      </Route>
      <Route path="404" component={PageNotFound} status="404" />
      <Route path="*" component={PageNotFound} status="404" />
    </Route>
  );
};

export default routes;
