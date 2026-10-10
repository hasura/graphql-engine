import { Route, Routes, Navigate } from 'react-router';
import { Main } from './components';
import { OAUTH_CALLBACK_URL } from '@hasura/shared/types';
import {
  PageNotFound,
  getActionsRouter,
  getRemoteSchemaRoutes,
  ApiExplorer,
  VoyagerView,
  App,
  MetadataContainer,
  MetadataOptions,
  MetadataStatus,
  Logout,
  About,
  ApiContainer,
  InheritedRolesContainer,
  ApiLimitsComponent,
  IntrospectionOptions,
  InsecureDomains,
  PrometheusSettings,
  QueryResponseCaching,
  OpenTelemetryFeature,
  MultipleAdminSecretsPage,
  MultipleJWTSecretsPage,
  SingleSignOnPage,
  SchemaRegistryContainer,
  getRestRoutes,
  getEventRoutes,
  getAllowListRoutes,
  makeDataRouter,
} from '@hasura/console-legacy-ce';
import AccessDeniedComponent from './components/AccessDenied/AccessDenied';
import OAuthCallback from './components/OAuthCallback/OAuthCallback';
import Login from './components/Login/Login';
import getMetricsRouter from './components/Services/Metrics/MetricsRouter';
import AuthProvider from './shared/auth/AuthProvider';
import PrivilegesRouteGuard from './components/RouteGuard/PrivilegesRouteGuard';
import DataRouteGuard from './components/RouteGuard/DataRouteGuard';
import { isMonitoringTabSupportedEnvironment } from '@hasura/shared/utils';

const Router = () => {
  return (
    <Routes>
      <Route
        path="/"
        element={
          <AuthProvider>
            <App />
          </AuthProvider>
        }
      >
        <Route path="login" element={<Login />} />
        <Route path={OAUTH_CALLBACK_URL} element={<OAuthCallback />} />
        <Route path="" element={<Main />}>
          <Route index element={<Navigate to="api/api-explorer" replace />} />
          <Route path="api" element={<ApiContainer />}>
            <Route index element={<Navigate to="api-explorer" replace />} />
            <Route path="api-explorer" element={<ApiExplorer />} />
            {getRestRoutes()}
            <Route
              path="schema-registry"
              element={<SchemaRegistryContainer />}
            />
            <Route
              path="schema-registry/:id"
              element={<SchemaRegistryContainer />}
            />
            {getAllowListRoutes()}
            <Route
              path=""
              element={
                <PrivilegesRouteGuard
                  deniedRoute="api/security/access_denied"
                  allowedPrivileges={['admin']}
                />
              }
            >
              <Route path="security" element={<ApiLimitsComponent />} />
              <Route
                path="security/api_limits"
                element={<ApiLimitsComponent />}
              />
              <Route
                path="security/introspection"
                element={<IntrospectionOptions />}
              />
            </Route>
            <Route
              path="security/access_denied"
              element={<AccessDeniedComponent />}
            />
          </Route>
          <Route path="voyager-view" element={<VoyagerView />} />
          <Route path="access-denied" element={<AccessDeniedComponent />} />
          {/* Disable monitoring tab when consoleType is pro-lite or oss */}
          {isMonitoringTabSupportedEnvironment(window.__env) ? (
            getMetricsRouter()
          ) : (
            <></>
          )}
          <Route path="" element={<DataRouteGuard />}>
            <Route path="settings" element={<MetadataContainer />}>
              <Route
                index
                element={<Navigate to="metadata-actions" replace />}
              />
              <Route
                path="schema-registry"
                element={<SchemaRegistryContainer />}
              />
              <Route
                path="schema-registry/:id"
                element={<SchemaRegistryContainer />}
              />
              <Route path="metadata-actions" element={<MetadataOptions />} />
              <Route path="metadata-status" element={<MetadataStatus />} />
              <Route path="logout" element={<Logout />} />
              <Route path="about" element={<About />} />
              <Route
                path="inherited-roles"
                element={<InheritedRolesContainer />}
              />
              <Route path="insecure-domain" element={<InsecureDomains />} />
              <Route
                path="prometheus-settings"
                element={<PrometheusSettings />}
              />
              <Route
                path="query-response-caching"
                element={<QueryResponseCaching />}
              />
              <Route
                path="multiple-admin-secrets"
                element={<MultipleAdminSecretsPage />}
              />
              <Route
                path="multiple-jwt-secrets"
                element={<MultipleJWTSecretsPage />}
              />
              <Route path="single-sign-on" element={<SingleSignOnPage />} />
              <Route path="opentelemetry" element={<OpenTelemetryFeature />} />
              {/* <Route path="feature-flags" element={<FeatureFlags />} /> */}
            </Route>
            {makeDataRouter()}
            {getActionsRouter()}
            {getEventRoutes()}
            {getRemoteSchemaRoutes()}
          </Route>
        </Route>
      </Route>
      <Route path="404" element={<PageNotFound />} />
      <Route path="*" element={<PageNotFound />} />
    </Routes>
  );
};

export default Router;
