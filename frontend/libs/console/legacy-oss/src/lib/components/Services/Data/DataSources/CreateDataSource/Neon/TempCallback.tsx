import * as React from 'react';
import { persistNeonCallbackSearch } from './utils';

/*
 * This component is only used for local development
 * It's used for listening to Neon's OAuth callback (/neon-integration/callback) in localdev.
 * In production, Neon's OAuth callback is handled by cloud dashboard
 */
export function NeonCallbackHandler() {
  React.useEffect(() => {
    // set the value for heroku callback search in local storage
    persistNeonCallbackSearch(window.location.search);
    window.close();
  }, []);

  return <>Please wait...</>;
}
