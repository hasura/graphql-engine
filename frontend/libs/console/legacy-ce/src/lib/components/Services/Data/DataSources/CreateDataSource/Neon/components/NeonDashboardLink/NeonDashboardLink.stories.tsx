import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import { NeonDashboardLink } from './NeonDashboardLink';

export default {
  title: 'features/Neon Integration/Neon Dashboard Link',
  component: NeonDashboardLink,
} as ComponentMeta<typeof NeonDashboardLink>;

export const Base: Story = () => <NeonDashboardLink />;
