import * as React from 'react';
import { persistHerokuCallbackSearch } from './utils';

/*
 * This component is only used for local development
 * It's used for listening to Heroku's OAuth callback (/heroku-callback) in localdev.
 * In production, Heroku's OAuth callback is handled by cloud dashboard
 */
export function HerokuCallbackHandler() {
  React.useEffect(() => {
    // set the value for heroku callback search in local storage
    persistHerokuCallbackSearch(window.location.search);
    window.close();
  }, []);

  return <>Please wait...</>;
}
