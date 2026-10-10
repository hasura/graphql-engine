import { useEffect } from 'react';
import {
  AppContext,
  defaultAppState,
  getEndpoints,
} from '@hasura/shared/context';
import { useServerVersion } from '@hasura/metadata/api';
import exportedGlobals from '../../Globals';
import { EnvVars } from '@hasura/shared/types';

const AppProvider = ({ children }: { children: React.ReactNode }) => {
  const envVars: EnvVars = {
    ...window.__env,
    urlPrefix: exportedGlobals.urlPrefix,
  };

  const endpoints = getEndpoints(window.__env, exportedGlobals.dataApiUrl);
  const serverVersionProps = useServerVersion({
    endpoints,
    envVars,
  });

  useEffect(() => {
    serverVersionProps.initServerVersion();
  }, []);

  return (
    <AppContext.Provider
      value={{
        ...defaultAppState,
        ...serverVersionProps,
        envVars,
        endpoints,
        isProduction: window.__env?.nodeEnv !== 'development',
      }}
    >
      {children}
    </AppContext.Provider>
  );
};

export default AppProvider;
