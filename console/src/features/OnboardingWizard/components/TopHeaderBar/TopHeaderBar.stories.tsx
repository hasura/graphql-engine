import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import { TopHeaderBar } from './TopHeaderBar';

export default {
  title: 'features/Onboarding Wizard/Top Header Bar',
  component: TopHeaderBar,
} as ComponentMeta<typeof TopHeaderBar>;

export const Base: Story = () => <TopHeaderBar />;
