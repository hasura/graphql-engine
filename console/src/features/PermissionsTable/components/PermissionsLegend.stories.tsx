import React from 'react';
import { Story, Meta } from '@storybook/react';

import { PermissionsLegend } from './PermissionsLegend';

export default {
  title: 'Permissions Table/Components/Permissions Legend',
  component: PermissionsLegend,
  parameters: { chromatic: { disableSnapshot: true } },
} as Meta;

export const Default: Story = () => <PermissionsLegend />;
