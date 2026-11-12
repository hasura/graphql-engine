import { requestJson } from '@hasura/shared/utils';
import { useAppContext, useAuthContext } from '@hasura/shared/context';
import { useLocation, useNavigate } from 'react-router';
import { HttpError, LOGIN_PATH, UnauthorizedError } from '@hasura/shared/types';
import { useCallback, useLayoutEffect, useRef } from 'react';

// HGE returns 400 for several authentication failures. Limit code-based
// detection to the engine metadata endpoint so a service's own access policy
// cannot inadvertently log the console user out.
const metadataAuthErrorCodes = new Set([
  'access-denied',
  'permission-denied',
  'invalid-jwt',
  'jwt-invalid-claims',
  'jwt-missing-role-claims',
]);

const isAuthenticationError = (
  error: unknown,
  url: string | URL,
  metadataUrl: string | undefined,
) => {
  if (!(error instanceof HttpError)) return false;
  if (error.status === 401) return true;
  if (error.status !== 400 && error.status !== 403) return false;
  if (!metadataUrl) return false;

  const requestUrl = new URL(url, window.location.href);
  const metadataEndpoint = new URL(metadataUrl, window.location.href);
  const isMetadata =
    requestUrl.origin === metadataEndpoint.origin &&
    requestUrl.pathname === metadataEndpoint.pathname;
  const code = error.data?.code;
  return (
    isMetadata && typeof code === 'string' && metadataAuthErrorCodes.has(code)
  );
};

export const useAuthFetchJson = () => {
  const location = useLocation();
  const navigate = useNavigate();
  const { getHeaders, logout } = useAuthContext();
  const { endpoints } = useAppContext();

  // The fetcher returned below has a STABLE identity across renders, so
  // consumers can list it in dependency arrays (and retain references to it)
  // without causing refetch loops. To still use current auth/router values, we
  // keep the latest *committed* `getHeaders`/`pathname`/`navigate` in a ref,
  // updated in a layout effect — never written during render, so there is no
  // concurrent-render state leakage, and it runs before any passive effect
  // (including children's) that may call the fetcher in the same commit. A plain `useCallback([getHeaders, ...])`
  // would NOT be stable because some auth providers (e.g. `useAdminSecretAuth`)
  // return a brand-new `getHeaders` closure every render; and a bare
  // `useCallback([])` closing over render values directly would go stale. This
  // ref is per hook instance (one per `useAuthFetchJson()` call), so auth
  // contexts never bleed between instances.
  const latestRef = useRef({
    getHeaders,
    logout,
    metadataUrl: endpoints.metadata,
    pathname: location.pathname,
    navigate,
  });
  useLayoutEffect(() => {
    latestRef.current = {
      getHeaders,
      logout,
      metadataUrl: endpoints.metadata,
      pathname: location.pathname,
      navigate,
    };
  }, [getHeaders, logout, endpoints.metadata, location.pathname, navigate]);

  return useCallback(
    async <T>(url: string | URL, config?: RequestInit): Promise<T> => {
      // Snapshot the latest committed values once per call so a single request
      // sees a consistent pathname/navigate/getHeaders (mirroring the previous
      // per-render closure), while still reflecting the most recent commit.
      const { getHeaders, logout, metadataUrl, pathname, navigate } =
        latestRef.current;

      let headers: Record<string, string> = {};
      try {
        headers = await getHeaders();
      } catch {
        if (pathname !== LOGIN_PATH) {
          logout();
          navigate(LOGIN_PATH, { replace: true });
          throw new UnauthorizedError();
        }
      }

      return requestJson<T>(url, {
        ...config,
        headers: {
          ...config?.headers,
          ...headers,
        },
      }).catch((err) => {
        if (
          isAuthenticationError(err, url, metadataUrl) &&
          pathname !== LOGIN_PATH
        ) {
          logout();
          navigate(LOGIN_PATH, { replace: true });
          throw new UnauthorizedError(err.message);
        }

        throw err;
      });
    },
    [],
  );
};
