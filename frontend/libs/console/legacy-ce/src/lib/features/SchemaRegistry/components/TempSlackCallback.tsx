import * as React from 'react';
import { persistSlackCallbackSearch } from '../utils';

/*
 * This component is only used for local development
 * It's used for listening to Slack's OAuth callback (/slack-integration/callback) in localdev.
 * In production, Slack's OAuth callback is handled by cloud dashboard
 */
export function SlackCallbackHandler() {
  React.useEffect(() => {
    // set the value for slack callback search in local storage
    persistSlackCallbackSearch(window.location.search);
    window.close();
  }, []);

  return <>Please wait...</>;
}
