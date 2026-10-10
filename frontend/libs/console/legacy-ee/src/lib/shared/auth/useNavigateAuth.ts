import { EnterpriseAuthState } from './types';
import { useLocation, useNavigate, useSearchParams } from 'react-router';
import { OAUTH_CALLBACK_URL, LOGIN_PATH } from '@hasura/shared/types';
import { constructRedirectUrl } from '../../utils/utils';
import { globals } from '@hasura/console-legacy-ce';

const useNavigateAuth = () => {
  const location = useLocation();
  const [searchParams] = useSearchParams();
  const navigate = useNavigate();

  return (state: EnterpriseAuthState | null) => {
    const isAuthenticated = Boolean(state);
    const isUnauthenticatedPath =
      location.pathname === LOGIN_PATH ||
      location.pathname === OAUTH_CALLBACK_URL;
    if (isAuthenticated) {
      if (isUnauthenticatedPath) {
        const redirectUrl = searchParams.get('redirect_url');
        const toUrl =
          (redirectUrl && redirectUrl !== 'undefined'
            ? `${globals.urlPrefix}${redirectUrl}`
            : globals.urlPrefix) || '/';
        navigate(toUrl, { replace: true });
      }

      return;
    }

    if (!isAuthenticated && !isUnauthenticatedPath) {
      // Check if a query param `auto_login=true` is set,
      // if it is set: pass it on
      const autoLogin =
        searchParams.get('auto_login') === 'true' ? '&auto_login=true' : '';

      navigate(
        {
          pathname: '/login',
          search: `?redirect_url=${window.encodeURIComponent(
            constructRedirectUrl(location.pathname, location.search),
          )}${autoLogin}`,
        },
        { replace: true },
      );
    }
  };
};

export default useNavigateAuth;
