import { useLocation, useSearchParams } from 'react-router';
import {
  getCurrentSsoIdentityProvider,
  getHasuraSsoIdentityProvider,
  getOAuthAuthorizeUrl,
  getOAuthRedirectUrl,
  initiateGeneralOAuthRequest,
} from '../../shared/auth/utils';
import { useEffect, useState } from 'react';
import { useAppContext } from '@hasura/shared/context';
import LoginEE from './LoginEE';
import { LoadingScreen, LoadingScreenError } from '@hasura/shared/ui';
import {
  clearConsoleAuthState,
  LoginContainer,
} from '@hasura/console-legacy-ce';

const Login = () => {
  const { envVars } = useAppContext();
  const location = useLocation();
  const [searchParams] = useSearchParams();
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(false);

  useEffect(() => {
    const isCloud = envVars.consoleType === 'cloud';
    const isAutoLogin = searchParams.get('auto_login') === 'true';

    if (isCloud || isAutoLogin) {
      // Cloud consoles always authenticate with Hasura, never with a provider
      // left in local storage by a self-hosted console.
      const idp = isCloud
        ? getHasuraSsoIdentityProvider(envVars)
        : getCurrentSsoIdentityProvider(envVars) ||
          getHasuraSsoIdentityProvider(envVars);

      if (idp) {
        const authorizeUrl = getOAuthAuthorizeUrl(
          idp.authorization_url,
          idp.client_id,
          idp.scope,
          getOAuthRedirectUrl(envVars.urlPrefix),
        );

        // The callback must validate the new code, not restore a rejected session.
        clearConsoleAuthState();
        initiateGeneralOAuthRequest(authorizeUrl, location, false);
        return;
      }

      // Do not offer admin-secret login when Cloud OAuth is misconfigured.
      if (isCloud) {
        setError(true);
        return;
      }
    }

    setLoading(false);
  }, []);

  if (error) {
    return (
      <LoadingScreen isError>
        <LoadingScreenError
          title="Hasura OAuth is not configured"
          message="The cloud console requires a Hasura OAuth client ID. Please contact support."
        />
      </LoadingScreen>
    );
  }

  if (loading) {
    return <LoadingScreen>Loading...</LoadingScreen>;
  }

  return (
    <LoginContainer>
      <LoginEE />
    </LoginContainer>
  );
};

export default Login;
