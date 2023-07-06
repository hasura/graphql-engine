import { Capabilities } from '@hasura/dc-api-types';
import {
  EnabledTabs,
  getEnabledTabs,
} from '../../../../features/Data/hooks/useEnabledTabs';
import { Feature } from '../../../../features/DataSource';

export function defaultTab(
  kind: string,
  capabilities: Feature | Capabilities | undefined
) {
  const enabledTabs = getEnabledTabs(kind, capabilities);
  const firstEnabledTab = Object.keys(enabledTabs).find(
    tab => enabledTabs[tab as keyof EnabledTabs]
  );
  return enabledTabs.browse ? 'browse' : firstEnabledTab;
}
