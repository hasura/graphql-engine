import { TabColor } from '../../../new-components/Tabs';

// this sets the tab colors for the Tabbed Manage Database UI only available via feature flag
export const TAB_COLORS = Object.freeze({
  primary: 'gray',
  secondary: 'blue',
}) satisfies Record<string, TabColor>;
