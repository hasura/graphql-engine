import React from 'react';
import { StoryFn, Meta } from '@storybook/react';
import { TopHeaderBar } from './TopHeaderBar';

export default {
  title: 'features/CloudOnboarding/Onboarding Wizard/Top Header Bar',
  component: TopHeaderBar,
} as Meta<typeof TopHeaderBar>;

export const Base: StoryFn = () => <TopHeaderBar />;
