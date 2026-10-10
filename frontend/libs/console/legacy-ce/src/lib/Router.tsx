import { Route, Routes, Navigate } from 'react-router';
import { App, Main, PageNotFound } from './components';
import Login from './components/Login/Login';
import { NeonCallbackHandler } from './features/CloudOnboarding/NeonOnboardingWizard/components/TempCallback';
import { SlackCallbackHandler } from './features/SchemaRegistry/components/TempSlackCallback';
import AuthProvider from './shared/auth/AuthProvider';
import getEventRoutes from './features/Eventing/routes';
import getActionRoutes from './features/Actions/routes';
import { ApiContainer, ApiExplorer } from './features/ApiExplorer';
import { getRestRoutes } from './features/RestEndpoints';
import { getAllowListRoutes } from './features/AllowLists';
import VoyagerView from './features/VoyagerView/components/VoyagerView';
import {
  About,
  InsecureDomains,
  Logout,
  MetadataOptions,
  MetadataStatus,
  SettingsContainer,
} from './features/Settings';
import InheritedRoles from './features/Settings/components/InheritedRoles/InheritedRoles';
import { getRemoteSchemaRoutes } from './features/RemoteSchema';
import makeDataRouter from './features/Data/routes';

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
        <Route
          path="neon-integration/callback"
          element={<NeonCallbackHandler />}
        />
        <Route
          path="slack-integration/callback"
          element={<SlackCallbackHandler />}
        />

        <Route path="" element={<Main />}>
          <Route index element={<Navigate to="api/api-explorer" replace />} />
          <Route path="api" element={<ApiContainer />}>
            <Route index element={<Navigate to="api-explorer" replace />} />
            <Route path="api-explorer" element={<ApiExplorer />} />
            {getRestRoutes()}
            {getAllowListRoutes()}
          </Route>

          <Route path="voyager-view" element={<VoyagerView />} />
          <Route path="settings" element={<SettingsContainer />}>
            <Route index element={<Navigate to="metadata-actions" replace />} />
            <Route path="metadata-actions" element={<MetadataOptions />} />
            <Route path="metadata-status" element={<MetadataStatus />} />
            <Route path="logout" element={<Logout />} />
            <Route path="about" element={<About />} />
            <Route path="inherited-roles" element={<InheritedRoles />} />
            <Route path="insecure-domain" element={<InsecureDomains />} />
            {/* <Route path="feature-flags" element={<FeatureFlags />} /> */}
          </Route>
          {makeDataRouter()}
          {getRemoteSchemaRoutes()}
          {getActionRoutes()}
          {getEventRoutes()}
        </Route>

        <Route path="404" element={<PageNotFound />} />
        <Route path="*" element={<PageNotFound />} />
      </Route>
    </Routes>
  );
};

export default Router;
