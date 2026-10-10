/**
 * THIS IS THE ENTRY POINT FOR THE CLIENT, JUST LIKE server.js IS THE ENTRY POINT FOR THE SERVER.
 */
import { RouteChangeListener, AppProvider } from '@hasura/console-legacy-ce';
import globals from './Globals';
import { BrowserRouter } from 'react-router';
import Router from './Router';
import { startTracing } from '@hasura/shared/analytics';
import { AppTheme } from '@hasura/shared/ui';
import { ReactQueryProvider } from '@hasura/metadata/api';

startTracing(window.__env);

/* ****************************************************************** */

// Main routes and rendering
export const Main = () => (
  <ReactQueryProvider>
    <AppProvider>
      <AppTheme>
        <BrowserRouter basename={globals.urlPrefix}>
          <RouteChangeListener />
          <Router />
        </BrowserRouter>
      </AppTheme>
    </AppProvider>
  </ReactQueryProvider>
);
