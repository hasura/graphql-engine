import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { RedirectCountDown } from './RedirectCountDown';

export default {
  title: 'features/CloudOnboarding/One Click Deployment/RedirectCountDown',
  component: RedirectCountDown,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof RedirectCountDown>;

export const Redirect: Story = () => (
  <RedirectCountDown
    timeSeconds={5}
    redirect={() => window.alert('redirect initiated')}
  />
);

export const CountDown: Story = () => (
  <RedirectCountDown
    timeSeconds={1000}
    redirect={() => window.alert('redirect initiated')}
  />
);
