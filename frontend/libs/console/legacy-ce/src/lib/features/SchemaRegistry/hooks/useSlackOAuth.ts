import * as React from 'react';
import { GraphQLError } from 'graphql';
import { useMemo, useState, useCallback } from 'react';
import { useQueryClient } from 'react-query';
import { FETCH_SLACK_STATE_QUERY_NAME } from '../constants';
import globals from '../../../Globals';
import { useIsUnmounted, generateRandomString } from '../utils';
import {
  getPersistedSlackCallbackSearch,
  clearPersistedSlackCallbackSearch,
} from '../utils';
import { controlPlaneClient } from '../../ControlPlane';
import { SLACK_TOKEN_EXCHANGE_QUERY } from '../queries';

export type ExchangeTokenResponse = {
  data?: {
    slackExchangeOAuthToken: {
      channel_name: string;
      team_name: string;
    };
  };
  errors?: GraphQLError[];
};

export type SlackOauthStatus =
  | {
      status: 'idle';
    }
  | {
      status: 'error';
      error: Error;
    }
  | {
      status: 'authenticating';
    }
  | {
      status: 'authenticated';
      channelName: string;
      teamName: string;
    };

export const useSlackOAuth = (oauthString?: string) => {
  const isUnmounted = useIsUnmounted();
  const queryClient = useQueryClient();

  const [status, setStatus] = useState<SlackOauthStatus>({
    status: 'idle',
  });

  const oauth2State = useMemo(
    () => oauthString || generateRandomString(),
    [oauthString]
  );

  /*
   * Handle the redirect params to get the oauth session:
   * */
  const startFetchingControlPlaneData = useCallback(
    async (callbackSearch: string) => {
      const params = new URLSearchParams(callbackSearch);

      // if grant code is absent, the redirect was unsuccessful
      if (!params.get('code')) {
        setStatus({
          status: 'error',
          error: new Error(
            'Error authenticating with Slack. Please try again.'
          ),
        });
        return;
      }

      /* if the state in params does not match the local state,
       * the request was probably tampered with
       * */
      if (params.get('state') !== oauth2State) {
        setStatus({
          status: 'error',
          error: new Error('Invalid OAuth session state. Please try again.'),
        });
        return;
      }

      try {
        const slackTokenExchangeResponse = await controlPlaneClient.query<
          ExchangeTokenResponse,
          { code: string; projectId: string }
        >(SLACK_TOKEN_EXCHANGE_QUERY, {
          code: params.get('code') || '',
          projectId: globals.hasuraCloudProjectId || '',
        });

        if (isUnmounted()) return;

        if (typeof slackTokenExchangeResponse === 'string') {
          setStatus({
            status: 'error',
            error: new Error(slackTokenExchangeResponse),
          });
          return;
        }

        if (
          slackTokenExchangeResponse.errors &&
          slackTokenExchangeResponse.errors.length > 0
        ) {
          throw Error(`Something went wrong while integrating Slack.`);
        }

        if (slackTokenExchangeResponse?.data) {
          queryClient.invalidateQueries(FETCH_SLACK_STATE_QUERY_NAME);
          setStatus({
            status: 'authenticated',
            channelName:
              slackTokenExchangeResponse.data.slackExchangeOAuthToken
                .channel_name,
            teamName:
              slackTokenExchangeResponse.data.slackExchangeOAuthToken.team_name,
          });
        }

        return;
      } catch (error) {
        console.error(error);

        setStatus({
          status: 'error',
          error: error instanceof Error ? error : new Error('Unknown error'),
        });
      }
    },
    [oauth2State]
  );

  // function to start the oauth process
  const startSlackOAuth = React.useCallback(() => {
    setStatus({ status: 'authenticating' });

    const searchParams = generateUrlSearchParams(
      globals.slackOAuthClientId ?? '',
      oauth2State
    );

    // open Slack auth page in a popup
    // Sample URL should look like this: https://slack.com/oauth/v2/authorize?scope=incoming-webhook&client_id=33336676.569200954261
    const popup = window.open(
      `https://${
        globals.slackRootDomain
      }/oauth/v2/authorize?${searchParams.toString()}`,
      'slack-oauth2',
      'menubar=no,toolbar=no,location=no,width=800,height=600'
    );

    // Usually means that a popup blocker blocked the popup
    if (!popup) {
      setStatus({
        status: 'error',
        error: new Error(
          'Could not open popup for logging in with Slack. Please disable your popup blocker and try again.'
        ),
      });
      return;
    }

    /*
     * After OAuth success, slack redirects the user to
     * /slack-integration/callback path of Hasura Cloud.
     * The Callback component passes the search params of the redirect
     * to the current context through localstorage.
     * We read the the localstorage and clear the redirect params.
     * */
    const intervalId = setInterval(() => {
      if (isUnmounted()) return;

      if (!popup.closed) return;
      clearInterval(intervalId);

      /*
       * Once the popup is closed,
       * the redirect params are expected to be available in localstorage. Check ../components/TempCallback
       * Here we read the params from localstorage and store them in local state.
       * These params are handled in another effect.
       * */
      setTimeout(() => {
        if (isUnmounted()) return;

        const search = getPersistedSlackCallbackSearch();
        if (!search) {
          setStatus({
            status: 'error',
            error: new Error(
              'Slack integration closed unexpectedly. Please try again.'
            ),
          });
          return;
        }

        startFetchingControlPlaneData(search);
        clearPersistedSlackCallbackSearch();
      }, 500);
    }, 500);
  }, [oauth2State, isUnmounted]);

  return {
    oauth2State,
    startSlackOAuth,
    slackOauthStatus: status,
  };
};

/*
  // Sample Slack pop-up window URL
  https://slack.com/oauth?
  client_id=254593026723.5374035727812&
  scope=incoming-webhook&
  user_scope=&
  redirect_uri=&
  state=&
  granular_bot_scope=1&
  single_channel=0&
  install_redirect=&
  tracked=1&
  team=
 */

function generateUrlSearchParams(
  slackOAuthClientId: string,
  oauth2State: string
) {
  const searchParams = new URLSearchParams();

  searchParams.set('client_id', slackOAuthClientId);
  searchParams.set('scope', 'incoming-webhook');
  searchParams.set('user_scope', '');
  searchParams.set('state', oauth2State);

  return searchParams;
}
