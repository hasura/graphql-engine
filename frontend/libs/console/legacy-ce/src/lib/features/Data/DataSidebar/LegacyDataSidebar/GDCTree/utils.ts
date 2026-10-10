import { Capabilities } from '@hasura/dc-api-types';
import { EnabledTabs, getEnabledTabs } from '../../../hooks/useEnabledTabs';

export function defaultTab(capabilities: Capabilities | undefined) {
  const enabledTabs = getEnabledTabs(capabilities);
  const firstEnabledTab = Object.keys(enabledTabs).find(
    (tab) => enabledTabs[tab as keyof EnabledTabs],
  );
  return enabledTabs.browse ? 'browse' : firstEnabledTab;
}
