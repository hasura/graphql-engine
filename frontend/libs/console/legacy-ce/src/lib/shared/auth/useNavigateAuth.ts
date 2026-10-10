import { useLocation, useNavigate } from 'react-router';
import { LOGIN_PATH } from '@hasura/shared/types';
import { useAppContext } from '@hasura/shared/context';

const useNavigateAuth = () => {
  const location = useLocation();
  const navigate = useNavigate();
  const { envVars } = useAppContext();

  return (isAuthenticated: boolean) => {
    if (isAuthenticated && location.pathname === LOGIN_PATH) {
      navigate(envVars.urlPrefix || '/', { replace: true });
      return;
    }

    if (!isAuthenticated && location.pathname !== LOGIN_PATH) {
      navigate(LOGIN_PATH, { replace: true });
    }
  };
};

export default useNavigateAuth;
