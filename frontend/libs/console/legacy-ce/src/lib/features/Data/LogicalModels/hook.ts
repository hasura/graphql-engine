import globals from '../../../Globals';
import { isProConsole } from '../../../utils';
import { nativeDrivers } from '../../DataSource';
import { useEELiteAccess } from '../../EETrial';

export const useSupportedDrivesForNativeQueries = () => {
  const { access: eeLiteAccess } = useEELiteAccess(globals);

  // this will tell us if console is pro or cloud
  const isPro = isProConsole(window.__env);

  if (eeLiteAccess === 'active' || isPro) return nativeDrivers;

  // for all other acess statuses, just return only postgres
  return ['postgres'];
};
