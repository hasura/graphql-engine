import { EnterpriseAuthState } from './types';
import { useLocation, useNavigate, useSearchParams } from 'react-router';
import { OAUTH_CALLBACK_URL, LOGIN_PATH } from '@hasura/shared/types';
import { constructRedirectUrl } from '../../utils/utils';
import { useAppContext } from '@hasura/shared/context';
import { getKeyFromLS } from './localStorage';
import { modifyRedirectUrl } from './utils';
import { getLoginRedirectPath } from './redirectPath';

const useNavigateAuth = () => {
  const location = useLocation();
  const [searchParams] = useSearchParams();
  const navigate = useNavigate();
  const { envVars } = useAppContext();

  return (state: EnterpriseAuthState | null) => {
    const isAuthenticated = Boolean(state);
    const isOAuthCallback = location.pathname === OAUTH_CALLBACK_URL;
    const isUnauthenticatedPath =
      location.pathname === LOGIN_PATH || isOAuthCallback;

    if (isAuthenticated) {
      if (isUnauthenticatedPath) {
        // OAuth providers do not echo the login page's redirect_url parameter.
        const redirectUrl = isOAuthCallback
          ? getKeyFromLS('redirect_url')
          : searchParams.get('redirect_url');
        const toUrl = getLoginRedirectPath(redirectUrl, envVars.urlPrefix);

        if (isOAuthCallback) {
          modifyRedirectUrl('/');
        }
        // BrowserRouter owns the basename; navigate only to a console-local path.
        navigate(toUrl, { replace: true });
      }

      return;
    }

    if (!isUnauthenticatedPath) {
      const autoLogin =
        searchParams.get('auto_login') === 'true' ? '&auto_login=true' : '';

      navigate(
        {
          pathname: LOGIN_PATH,
          search: `?redirect_url=${window.encodeURIComponent(
            constructRedirectUrl(location.pathname, location.search) +
              location.hash,
          )}${autoLogin}`,
        },
        { replace: true },
      );
    }
  };
};

export default useNavigateAuth;
