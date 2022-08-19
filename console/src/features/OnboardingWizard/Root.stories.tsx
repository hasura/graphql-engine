import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import { Root } from './Root';

export default {
  title: 'features/Onboarding Wizard/Root',
  component: Root,
} as ComponentMeta<typeof Root>;

export const Base: Story = () => <Root />;
