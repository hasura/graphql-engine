import { addons, types } from 'storybook/manager-api';

import { ADDON_ID, PANEL_ID } from '../constants';
import { Panel } from '../Panel';

// Register the addon
addons.register(ADDON_ID, () => {
  // Register the panel
  addons.add(PANEL_ID, {
    type: types.PANEL,
    title: 'Console Env',
    match: ({ viewMode }: { viewMode?: string }) => viewMode === 'story',
    render: Panel,
  });
});
