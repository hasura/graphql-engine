import type { ReactNode } from 'react';
import { Navigate } from 'react-router';
import { useAuthContext } from '@hasura/shared/context';
import { LOGIN_PATH } from '@hasura/shared/types';

// Public login/OAuth routes remain outside this boundary. In particular, an
// auth failure must unmount the console, not merely change the browser URL.
export const AuthenticatedRoute = ({ children }: { children: ReactNode }) => {
  const { isAuthenticated } = useAuthContext();
  return isAuthenticated ? children : <Navigate to={LOGIN_PATH} replace />;
};
