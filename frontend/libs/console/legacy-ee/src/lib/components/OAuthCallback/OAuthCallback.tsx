import { useEffect, useState } from 'react';
import { useSearchParams } from 'react-router';
import { validateOauthResponseState, defaultErrorMessage } from './utils';
import {
  getCurrentSsoIdentityProvider,
  retrieveIdToken,
} from '../../shared/auth/utils';
import useSSOAuth from '../../shared/auth/useSsoAuth';
import useNavigateAuth from '../../shared/auth/useNavigateAuth';
import {
  DisplayToastErrorMessage,
  LoadingScreen,
  LoadingScreenError,
  LoadingScreenTitle,
} from '@hasura/shared/ui';
import { useAppContext } from '@hasura/shared/context';
import { Code } from '@radix-ui/themes';

const OAuthCallback = () => {
  const { envVars } = useAppContext();
  const [searchParams] = useSearchParams();
  const { loginSSO, loginHasuraSSO } = useSSOAuth();
  const navigateAuth = useNavigateAuth();
  const [errorMsg, setErrorMsg] = useState({ ...defaultErrorMessage });

  const verificationError = (err: typeof defaultErrorMessage) => {
    setErrorMsg(err);
  };

  useEffect(() => {
    const code = searchParams.get('code');
    const state = searchParams.get('state');

    if (code && validateOauthResponseState(state)) {
      const idp = getCurrentSsoIdentityProvider(envVars);
      if (!idp) {
        return verificationError({
          error: 'Invalid SSO Provider',
          error_description:
            'Unexpected error - You could face this issue if the server is not running with a correct `HASURA_GRAPHQL_SSO_PROVIDERS`',
        });
      }

      void retrieveIdToken(idp, code, envVars.urlPrefix)
        .then((data) => {
          // continue to process the EE lux authorization flow
          // if the current client_id equals the Hasura OAuth Client ID in the global config
          // otherwise fallback to the external SSO OAuth flow
          if (envVars.consoleId && idp.client_id === envVars.consoleId) {
            return loginHasuraSSO(data);
          }

          return loginSSO(idp, data);
        })
        .then((state) => {
          if (!state) {
            return verificationError({
              error: 'Invalid ID Token',
              error_description:
                'Hasura SSO supports JWT-compatible ID Token only',
            });
          }

          navigateAuth(state);
        })
        .catch((err) => {
          verificationError({
            error: 'OAuth Login Failed',
            error_description: <DisplayToastErrorMessage message={err} />,
          });
        });

      return;
    }

    const err = {
      error: searchParams.get('error') || 'State verification failed',
      error_description: searchParams.get('error_description') || (
        <>
          Unexpected error - You could face this issue if the server is not
          running with a correct <Code color="red">HASURA_GRAPHQL_PRO_KEY</Code>
        </>
      ),
    };
    verificationError(err);
  }, []);

  return (
    <LoadingScreen isError={Boolean(errorMsg.error)}>
      {errorMsg.error ? (
        <LoadingScreenError
          title={errorMsg.error}
          message={errorMsg.error_description}
          link="/login"
          linkText="Back to Login"
        />
      ) : (
        <LoadingScreenTitle title="Validating..." />
      )}
    </LoadingScreen>
  );
};

export default OAuthCallback;
