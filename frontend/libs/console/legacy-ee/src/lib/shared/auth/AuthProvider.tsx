import { AuthContext, type AuthService } from '@hasura/shared/context';
import useEnterpriseAuth from './useEnterpriseAuth';
import { useEffect, useRef, useState } from 'react';
import { LoadingScreen, LoadingScreenTitle } from '@hasura/shared/ui';

const AuthProvider = ({ children }: { children: React.ReactNode }) => {
  const [initializing, setInitializing] = useState(true);
  // the auth context is recreated on every render, so re-renders during initialization
  // would start it again and send the same refresh token concurrently.
  const initializeStartedRef = useRef(false);
  const authContext = useEnterpriseAuth();

  useEffect(() => {
    if (initializeStartedRef.current || !authContext) {
      return;
    }

    initializeStartedRef.current = true;
    authContext.initialize().finally(() => setInitializing(false));
  }, [authContext]);

  return initializing ? (
    <LoadingScreen>
      <LoadingScreenTitle title="Validating..." />
    </LoadingScreen>
  ) : (
    <AuthContext.Provider value={authContext as AuthService<any>}>
      {children}
    </AuthContext.Provider>
  );
};

export default AuthProvider;
