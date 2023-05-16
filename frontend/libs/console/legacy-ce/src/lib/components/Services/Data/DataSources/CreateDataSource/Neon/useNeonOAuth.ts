import * as React from 'react';
import { useMemo, useState, useCallback } from 'react';
import globals from '../../../../../../Globals';
import { createFetchControlPlaneData } from '../../../../../../hooks/createFetchControlPlaneData';
import { generateRandomString } from '../utils';
import {
  getPersistedNeonCallbackSearch,
  clearPersistedNeonCallbackSearch,
} from './utils';
import { useIsUnmounted } from './useIsUnmounted';

export type ExchangeTokenResponse = {
  data: {
    neonExchangeOAuthToken: {
      accessToken: string;
      email: string;
    };
  };
};

const NEON_TOKEN_EXCHANGE_QUERY = `
  mutation neonTokenExchange (
    $code: String!
    $state: String!
    $projectId: uuid!
  ) {
    neonExchangeOAuthToken (
      code: $code
      state: $state
      projectId: $projectId
    ) {
      accessToken
      email
    }
  }
`;

type NeonOauthStatus =
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
      email: string;
    };

export const useNeonOAuth = (oauthString?: string) => {
  const isUnmounted = useIsUnmounted();

  const [status, setStatus] = useState<NeonOauthStatus>({
    status: 'idle',
  });

  // initialise the GraphQL query to exchange oauth token
  const fetchControlPlaneData = useMemo(() => {
    return createFetchControlPlaneData<ExchangeTokenResponse>({
      query: NEON_TOKEN_EXCHANGE_QUERY,
    });
  }, []);

  // generates a local oauth state to avoid forged callback
  // FIXME we should not have conditional hooks
  // Suggested fix : useMemo(() => oauthString ? oauthString : generateRandomString(), [oauthString])
  // eslint-disable-next-line react-hooks/rules-of-hooks
  const oauth2State = oauthString ?? useMemo(generateRandomString, []);

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
          error: new Error('Error authenticating with Neon. Please try again.'),
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
        const responseOrError = await fetchControlPlaneData({
          variables: {
            code: params.get('code') || '',
            state: params.get('state') || '',
            projectId: globals.hasuraCloudProjectId || '',
          },
        });

        if (isUnmounted()) return;

        if (typeof responseOrError === 'string') {
          setStatus({
            status: 'error',
            error: new Error(responseOrError),
          });
          return;
        }

        const response = responseOrError;
        setStatus({
          status: 'authenticated',
          email: response.data.neonExchangeOAuthToken.email,
        });
        return;
      } catch (error) {
        console.error(error);

        setStatus({
          status: 'error',
          error: error instanceof Error ? error : new Error('Unknown error'),
        });
      }
    },
    [oauth2State, fetchControlPlaneData]
  );

  // function to start the oauth process
  const startNeonOAuth = React.useCallback(() => {
    setStatus({ status: 'authenticating' });

    const searchParams = generateUrlSearchParams(
      globals.neonOAuthClientId ?? '',
      `${window.location.origin}/neon-integration/callback`,
      oauth2State
    );

    // open Neon auth page in a popup
    const popup = window.open(
      `https://oauth2.${
        globals.neonRootDomain
      }/oauth2/auth?${searchParams.toString()}`,
      'neon-oauth2',
      'menubar=no,toolbar=no,location=no,width=800,height=600'
    );

    // Usually means that a popup blocked blocked the popup
    if (!popup) {
      setStatus({
        status: 'error',
        error: new Error(
          'Could not open popup for logging in with Neon. Please disable your popup blocker and try again.'
        ),
      });
      return;
    }

    /*
     * After OAuth success, neon redirects the user to
     * /neon-integration/callback path of Hasura Cloud. Check ./TempNeonCallback.
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
       * the redirect params are expected to be available in localstorage.
       * Here we read the params from localstorage and store them in local state.
       * These params are handled in another effect.
       * */
      setTimeout(() => {
        if (isUnmounted()) return;

        const search = getPersistedNeonCallbackSearch();
        if (!search) {
          setStatus({
            status: 'error',
            error: new Error(
              'Neon login closed unexpectedly. Please try again.'
            ),
          });
          return;
        }

        startFetchingControlPlaneData(search);
        clearPersistedNeonCallbackSearch();
      }, 500);
    }, 500);
  }, [oauth2State, isUnmounted]);

  return {
    oauth2State,
    startNeonOAuth,
    neonOauthStatus: status,
  };
};

function generateUrlSearchParams(
  neonOAuthClientId: string,
  redirectURI: string,
  oauth2State: string
) {
  const searchParams = new URLSearchParams();

  searchParams.set('client_id', neonOAuthClientId);
  searchParams.set('redirect_uri', redirectURI);
  searchParams.set('response_type', 'code');
  searchParams.set(
    'scope',
    'openid offline urn:neoncloud:projects:create urn:neoncloud:projects:read urn:neoncloud:projects:update urn:neoncloud:projects:delete'
  );
  searchParams.set('state', oauth2State);

  return searchParams;
}
