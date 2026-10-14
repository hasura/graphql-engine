import { useLocation, useNavigate } from 'react-router';
import { LOGIN_PATH } from '@hasura/shared/types';
import { useCallback } from 'react';

const useNavigateAuth = () => {
  const location = useLocation();
  const navigate = useNavigate();

  return useCallback(
    (isAuthenticated: boolean) => {
      if (isAuthenticated && location.pathname === LOGIN_PATH) {
        navigate('/', { replace: true });
        return;
      }

      if (!isAuthenticated && location.pathname !== LOGIN_PATH) {
        navigate(LOGIN_PATH, { replace: true });
      }
    },
    [navigate, location],
  );
};

export default useNavigateAuth;
