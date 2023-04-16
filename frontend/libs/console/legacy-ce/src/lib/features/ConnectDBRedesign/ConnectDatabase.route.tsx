import { ConnectDatabaseV2 } from './ConnectDatabase';
import { useEnvironmentState } from './hooks/useEnvironmentState';

/**
 *
 * This is a wrapper component intended to be used directly as a route
 *
 */
export const ConnectDatabaseRouteWrapper = () => {
  const env = useEnvironmentState();
  return <ConnectDatabaseV2 {...env} />;
};
