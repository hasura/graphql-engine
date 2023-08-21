import globals from '../../../Globals';
import { isObject } from '../../../components/Common/utils/jsUtils';
import { isProConsole } from '../../../utils';
import { Feature } from '../../DataSource';
import { useEELiteAccess } from '../../EETrial';
import { useAllDriverCapabilities } from '../hooks/useAllDriverCapabilities';

export const useSupportedDriversForNativeQueries = () => {
  const { data: supportedDrivers = [] } = useAllDriverCapabilities({
    select: data => {
      return data
        .filter(item => {
          if (item.capabilities === Feature.NotImplemented) return false;

          return isObject(item.capabilities.interpolated_queries);
        })
        .map(item => item.driver.kind);
    },
  });
  const { access: eeLiteAccess } = useEELiteAccess(globals);

  // this will tell us if console is pro or cloud
  const isPro = isProConsole(window.__env);

  // Hardcoded list until we have a proper way to determine from capabilities
  if (eeLiteAccess === 'active' || isPro) return supportedDrivers;

  // for all other acess statuses, just return only postgres
  return ['postgres'];
};
