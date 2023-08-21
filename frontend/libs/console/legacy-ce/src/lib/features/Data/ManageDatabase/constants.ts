import { TabColor } from '../../../new-components/Tabs';

// this sets the tab colors for the Tabbed Manage Database UI only available via feature flag
export const TAB_COLORS = Object.freeze({
  topLevel: 'blue',
  trackingLevel: 'purple',
}) satisfies Record<string, TabColor>;
