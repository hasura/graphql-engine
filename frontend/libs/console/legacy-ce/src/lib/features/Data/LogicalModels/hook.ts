import globals from '../../../Globals';
import { isProConsole } from '../../../utils';
import { nativeDrivers } from '../../DataSource';
import { useEELiteAccess } from '../../EETrial';

export const useSupportedDrivesForNativeQueries = () => {
  const allowedDriversForCE = ['postgres'];

  const { access: eeLiteAccess } = useEELiteAccess(globals);

  // this will tell us if console is pro or cloud
  const isPro = isProConsole(window.__env);

  if (eeLiteAccess === 'active' || isPro) return nativeDrivers;

  // this is to return nothing for oss
  if (eeLiteAccess === 'forbidden') return allowedDriversForCE;

  return allowedDriversForCE;
};
