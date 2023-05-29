import React from 'react';
import { StoryFn, Meta } from '@storybook/react';
import { NeonDashboardLink } from './NeonDashboardLink';

export default {
  title: 'features/Neon Integration/Neon Dashboard Link',
  component: NeonDashboardLink,
} as Meta<typeof NeonDashboardLink>;

export const Base: StoryFn = () => <NeonDashboardLink />;
