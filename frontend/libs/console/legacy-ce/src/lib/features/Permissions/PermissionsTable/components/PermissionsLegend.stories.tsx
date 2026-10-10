import React from 'react';
import { StoryFn, Meta } from '@storybook/react-webpack5';

import { PermissionsLegend } from './PermissionsLegend';

export default {
  component: PermissionsLegend,
  parameters: { chromatic: { disableSnapshot: true } },
} as Meta;

export const Default: StoryFn = () => <PermissionsLegend />;
