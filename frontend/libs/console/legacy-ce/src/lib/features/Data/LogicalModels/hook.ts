import globals from '../../../Globals';
import { isProConsole } from '../../../utils';
import { nativeDrivers } from '../../DataSource';
import { useEELiteAccess } from '../../EETrial';

export const useSupportedDrivesForNativeQueries = () => {
  const allowedDriversForCE = ['postgres'];

  const { access: eeLiteAccess } = useEELiteAccess(globals);

  /**
   * There are three cases here.
   * 1. If it's OSS - do not show the children at all. (there is no point in using this wrapper for oss features)
   * 2. If it's pro lite
   *   - show the "Try pro-lite" license form if license is not active.
   *   - show the children if license is active.
   * 3. If it's cloud/pro just show the children
   *
   */

  // this will tell us if console is pro or cloud
  const isPro = isProConsole(window.__env);

  if (eeLiteAccess === 'active' || isPro) return nativeDrivers;

  // this is to return nothing for oss
  if (eeLiteAccess === 'forbidden') return allowedDriversForCE;

  return allowedDriversForCE;
};
