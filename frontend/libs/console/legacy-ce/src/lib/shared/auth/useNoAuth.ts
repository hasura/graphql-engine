import { useCallback, useState } from 'react';
import { useQueryClient } from '@tanstack/react-query';
import { AuthService } from '@hasura/shared/context';
import useNavigateAuth from './useNavigateAuth';
import { useFetchInconsistentMetadata } from '@hasura/metadata/api';

const getHeaders = async () => ({});

/**
 * The noAuth mode is used if the server does not set admin secret.
 */
export const useNoAuth = (): AuthService<any> => {
  const navigateAuth = useNavigateAuth();
  const queryClient = useQueryClient();
  const fetchInconsistentMetadata = useFetchInconsistentMetadata();
  const [isAuthenticated, setIsAuthenticated] = useState(false);

  const authenticate = useCallback(async () => {
    try {
      await fetchInconsistentMetadata({});
      setIsAuthenticated(true);

      return true;
    } catch (err) {
      // Use a functional update so we don't need `isAuthenticated` as a
      // dependency (and avoid a stale closure). Setting the same value is a
      // no-op in React, matching the previous guarded behavior.
      setIsAuthenticated((prev) => (prev ? false : prev));

      throw err;
    }
  }, [fetchInconsistentMetadata]);

  const initialize = async () => {
    await authenticate()
      .then(navigateAuth)
      .catch(() => navigateAuth(false));
  };

  const logout = () => {
    setIsAuthenticated(false);
    queryClient.clear();
  };

  return {
    authType: 'none',
    authenticate,
    isAuthenticated,
    getHeaders,
    initialize,
    logout,
  };
};
