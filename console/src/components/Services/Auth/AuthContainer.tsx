import { ENABLE_AUTH_LAYER } from '@/utils/featureFlags';
import React from 'react';
import { AuthContextProvider } from './useAuthContext';

const AuthContainer = ({ children }: { children: React.ReactNode }) => {
  if (!ENABLE_AUTH_LAYER) return <>{children}</>;

  return (
    <AuthContextProvider>{children}</AuthContextProvider>
  );
};

export default AuthContainer;
