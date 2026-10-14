import { useEffect, useRef } from 'react';
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

  // Kick off the one-time server-version fetch on mount only. `initServerVersion`
  // is a fresh closure every render (it isn't memoized in useServerVersion), so
  // we capture the mount-render instance in a ref and invoke it exactly once,
  // rather than listing the ever-changing object in the dependency array and
  // re-running the fetch on every render.
  const initServerVersionRef = useRef(serverVersionProps.initServerVersion);
  useEffect(() => {
    initServerVersionRef.current();
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
