import { useCallback, useState } from 'react';
import { AuthService } from '@hasura/shared/context';
import useNavigateAuth from './useNavigateAuth';
import { useFetchInconsistentMetadata } from '@hasura/metadata/api';

/**
 * The noAuth mode is used if the server does not set admin secret.
 */
export const useNoAuth = (): AuthService<any> => {
  const navigateAuth = useNavigateAuth();
  const fetchInconsistentMetadata = useFetchInconsistentMetadata();
  const [isAuthenticated, setIsAuthenticated] = useState(false);

  const authenticate = useCallback(async () => {
    try {
      await fetchInconsistentMetadata({});
      setIsAuthenticated(true);

      return true;
    } catch (err) {
      if (isAuthenticated) {
        setIsAuthenticated(false);
      }

      throw err;
    }
  }, [setIsAuthenticated]);

  const initialize = async () => {
    await authenticate().then(navigateAuth);
  };

  const logout = () => {
    setIsAuthenticated(false);
  };

  return {
    authType: 'none',
    authenticate,
    isAuthenticated,
    getHeaders: async () => ({}),
    initialize,
    logout,
  };
};
