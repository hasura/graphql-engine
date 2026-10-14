import { requestJson } from '@hasura/shared/utils';
import { useAuthContext } from '@hasura/shared/context';
import { useLocation, useNavigate } from 'react-router';
import { HttpError, LOGIN_PATH, UnauthorizedError } from '@hasura/shared/types';
import { useCallback, useLayoutEffect, useRef } from 'react';

export const useAuthFetchJson = () => {
  const location = useLocation();
  const navigate = useNavigate();
  const { getHeaders } = useAuthContext();

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
    pathname: location.pathname,
    navigate,
  });
  useLayoutEffect(() => {
    latestRef.current = { getHeaders, pathname: location.pathname, navigate };
  }, [getHeaders, location.pathname, navigate]);

  return useCallback(
    async <T>(url: string | URL, config?: RequestInit): Promise<T> => {
      // Snapshot the latest committed values once per call so a single request
      // sees a consistent pathname/navigate/getHeaders (mirroring the previous
      // per-render closure), while still reflecting the most recent commit.
      const { getHeaders, pathname, navigate } = latestRef.current;

      let headers: Record<string, string> = {};
      try {
        headers = await getHeaders();
      } catch {
        if (pathname !== LOGIN_PATH) {
          navigate(LOGIN_PATH);
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
          err instanceof HttpError &&
          err.status === 401 &&
          pathname !== LOGIN_PATH
        ) {
          navigate(LOGIN_PATH);
          throw new UnauthorizedError(err.message);
        }

        throw err;
      });
    },
    [],
  );
};
