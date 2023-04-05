import globals from '../../../Globals';

import { isCloudConsole } from '../../../utils';
import { isEECloud } from '../../../utils/cloudConsole';
import { useEELiteAccess } from '../../EETrial';
import { DbConnectConsoleType } from '../types';

const determineConsoleType = (): DbConnectConsoleType => {
  const hasuraCloud = isCloudConsole(globals);
  const eeCloud = isEECloud(globals);

  if (globals.consoleType === 'pro-lite') {
    return 'pro-lite';
  } else if (globals.consoleType === 'oss') {
    return 'oss';
  } else if (hasuraCloud || eeCloud) {
    return 'cloud';
  } else {
    return 'pro';
  }
};

export const useEnvironmentState = () => {
  // isPro is pro + cloud (both self-hosted && hasura cloud)
  const { access: eeLicenseInfo } = useEELiteAccess(globals);

  return {
    eeLicenseInfo,
    consoleType: determineConsoleType(),
  };
};
