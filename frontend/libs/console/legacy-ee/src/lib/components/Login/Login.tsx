import { useSearchParams } from 'react-router';
import {
  getCurrentSsoIdentityProvider,
  getHasuraSsoIdentityProvider,
  getOAuthAuthorizeUrl,
  getOAuthRedirectUrl,
  modifyRedirectUrl,
} from '../../shared/auth/utils';
import { useEffect, useState } from 'react';
import { useAppContext } from '@hasura/shared/context';
import LoginEE from './LoginEE';
import { LoadingScreen } from '@hasura/shared/ui';
import { LoginContainer } from '@hasura/console-legacy-ce';

const Login = () => {
  const { envVars } = useAppContext();
  const [searchParams] = useSearchParams();
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    const isAutoLogin = searchParams.get('auto_login') === 'true';

    if (isAutoLogin) {
      const idp =
        getCurrentSsoIdentityProvider(envVars) ||
        getHasuraSsoIdentityProvider(envVars);

      if (idp) {
        const redirectUri = getOAuthRedirectUrl(envVars.urlPrefix);
        const authorizeUrl = getOAuthAuthorizeUrl(
          idp.authorization_url,
          idp.client_id,
          idp.scope,
          redirectUri,
        );

        if (isAutoLogin) {
          modifyRedirectUrl('/');
          window.location.href = authorizeUrl;

          return;
        }

        const redirectUrl = searchParams.get('redirect_url');

        if (redirectUrl && redirectUrl !== 'undefined') {
          modifyRedirectUrl(decodeURIComponent(redirectUrl));
        } else {
          modifyRedirectUrl('/');
        }

        window.location.href = authorizeUrl;
        return;
      }
    }

    setLoading(false);
  }, []);

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
