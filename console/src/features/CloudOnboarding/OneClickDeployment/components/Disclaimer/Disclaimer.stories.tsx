import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import { Disclaimer } from './Disclaimer';

export default {
  title: 'features/CloudOnboarding/One Click Deployment/Disclaimer',
  component: Disclaimer,
} as ComponentMeta<typeof Disclaimer>;

export const Base: Story = () => <Disclaimer />;
