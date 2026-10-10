import { isEECloud, isCloudConsole } from '@hasura/shared/utils';
import { useEELiteAccess } from '../../EETrial';
import { DbConnectConsoleType } from '../types';
import { useAppContext } from '@hasura/shared/context';

export const useEnvironmentState = () => {
  // isPro is pro + cloud (both self-hosted && hasura cloud)
  const { access: eeLicenseInfo } = useEELiteAccess();
  const { envVars } = useAppContext();

  const determineConsoleType = (): DbConnectConsoleType => {
    const hasuraCloud = isCloudConsole(envVars);
    const eeCloud = isEECloud(envVars);

    if (envVars.consoleType === 'pro-lite') {
      return 'pro-lite';
    } else if (envVars.consoleType === 'oss') {
      return 'oss';
    } else if (hasuraCloud || eeCloud) {
      return 'cloud';
    } else {
      return 'pro';
    }
  };

  return {
    eeLicenseInfo,
    consoleType: determineConsoleType(),
  };
};
