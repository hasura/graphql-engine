import React from 'react';
import { StoryFn, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { RedirectCountDown } from './RedirectCountDown';

export default {
  title: 'features/CloudOnboarding/One Click Deployment/RedirectCountDown',
  component: RedirectCountDown,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof RedirectCountDown>;

export const Redirect: StoryFn = () => (
  <RedirectCountDown
    timeSeconds={5}
    redirect={() => window.alert('redirect initiated')}
  />
);

export const CountDown: StoryFn = () => (
  <RedirectCountDown
    timeSeconds={1000}
    redirect={() => window.alert('redirect initiated')}
  />
);
