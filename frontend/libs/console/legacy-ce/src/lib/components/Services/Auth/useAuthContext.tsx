import globals from '../../../Globals';
import { useConsoleConfig } from '../../../hooks/useEnvVars';
import React, { createContext, useContext } from 'react';
import { browserHistory } from 'react-router';
import { getAdminSecret } from '../ApiExplorer/ApiRequest/utils';
import { AllowedConsoleModes, AuthService } from './AuthService';
import { OSSAuthService } from './OSSAuthService';

function useAuthContextValue() {
  // get the auth service object
  const {
    mode: consoleMode,
    is_admin_secret_set: isAdminSecretSet,
    type: consoleType,
  } = useConsoleConfig();

  let authService: AuthService | undefined;

  if (consoleType === 'oss' && consoleMode === 'server') {
    if (
      !isAdminSecretSet ||
      (isAdminSecretSet &&
        (Boolean(getAdminSecret()) || getAdminSecret() === null))
    ) {
      const ossAuthService = new OSSAuthService({
        consoleMode: globals.consoleMode as AllowedConsoleModes,
        consoleType: 'oss',
      });
      const adminSecret = getAdminSecret();
      ossAuthService.authenticate(adminSecret);
      authService = ossAuthService;
    }
  }

  if (consoleType === 'oss' && consoleMode === 'cli') {
    const ossAuthService = new OSSAuthService({
      consoleMode: globals.consoleMode as AllowedConsoleModes,
      consoleType: 'oss',
    });
    const adminSecret = getAdminSecret();
    ossAuthService.authenticate(adminSecret);
    authService = ossAuthService;
  }

  return {
    isProjectAdmin: authService?.isProjectAdmin,
    isCollaborator: authService?.isCollaborator,
    hasPrivilege: authService?.hasPrivilege,
    isAuthenticated: authService !== undefined,
  };
}

const AuthContext = createContext<
  ReturnType<typeof useAuthContextValue> | undefined
>(undefined);

// export const useAuth = () => useContext(Context);

export const AuthContextProvider: React.FC = props => {
  const value = useAuthContextValue();
  if (value.isAuthenticated)
    return <AuthContext.Provider value={value} {...props} />;

  browserHistory.push('/login');
  return null;
};

export function useAuthContext() {
  const context = useContext(AuthContext);
  if (context === undefined) {
    throw new Error(`useAuthContext must be used within a AuthContextProvider`);
  }
  return {
    ...context,
  };
}
