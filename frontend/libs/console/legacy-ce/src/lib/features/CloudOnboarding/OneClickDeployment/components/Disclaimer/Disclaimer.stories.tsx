import React from 'react';
import { StoryFn, Meta } from '@storybook/react';
import { Disclaimer } from './Disclaimer';

export default {
  title: 'features/CloudOnboarding/One Click Deployment/Disclaimer',
  component: Disclaimer,
} as Meta<typeof Disclaimer>;

export const Base: StoryFn = () => <Disclaimer />;
