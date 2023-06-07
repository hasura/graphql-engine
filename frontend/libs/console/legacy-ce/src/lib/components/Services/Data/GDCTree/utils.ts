import {
  EnabledTabs,
  getEnabledTabs,
} from '../../../../features/Data/hooks/useEnabledTabs';

export function defaultTab(kind: string) {
  const enabledTabs = getEnabledTabs(kind);
  const firstEnabledTab = Object.keys(enabledTabs).find(
    tab => enabledTabs[tab as keyof EnabledTabs]
  );
  return enabledTabs.browse ? 'browse' : firstEnabledTab;
}
