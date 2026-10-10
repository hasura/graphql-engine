import { OAuthTokenResponse, SsoIdentityProvider } from '@hasura/shared/types';
import { useAuthContext } from './context';
import type { EnterpriseAuthState } from './types';
import { makeHasuraSsoAuthState, makeSsoAuthState } from './utils';
import { useAppContext } from '@hasura/shared/context';

const useSSOAuth = () => {
  const { envVars } = useAppContext();
  const auth = useAuthContext();

  const loginSSO = async (
    provider: SsoIdentityProvider,
    data: OAuthTokenResponse,
  ): Promise<EnterpriseAuthState | null> => {
    const authState = makeSsoAuthState(provider, data);
    if (!authState) {
      return null;
    }

    // fetch server config to check if the current token has admin role
    const isAuthenticated = await auth.authenticate(authState);

    return isAuthenticated ? authState : null;
  };

  const loginHasuraSSO = async (
    data: OAuthTokenResponse,
  ): Promise<EnterpriseAuthState | null> => {
    const authState = makeHasuraSsoAuthState(data, envVars);
    if (!authState) {
      return null;
    }

    // fetch server config to check if the current token has admin role
    const isAuthenticated = await auth.authenticate(authState);

    return isAuthenticated ? authState : null;
  };

  return {
    loginSSO,
    loginHasuraSSO,
  };
};

export default useSSOAuth;
