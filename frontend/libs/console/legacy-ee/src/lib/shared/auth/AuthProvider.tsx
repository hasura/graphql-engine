import { AuthContext, type AuthService } from '@hasura/shared/context';
import useEnterpriseAuth from './useEnterpriseAuth';
import { useEffect, useState } from 'react';
import { LoadingScreen, LoadingScreenTitle } from '@hasura/shared/ui';

const AuthProvider = ({ children }: { children: React.ReactNode }) => {
  const [initializing, setInitializing] = useState(true);
  const authContext = useEnterpriseAuth();

  useEffect(() => {
    if (!initializing || !authContext) {
      return;
    }

    authContext.initialize().then(() => {
      setInitializing(false);
    });
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
