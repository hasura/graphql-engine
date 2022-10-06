import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import { HerokuBanner } from './Banner';

export default {
  title: 'features/Neon Integration/Heroku Banner',
  component: HerokuBanner,
} as ComponentMeta<typeof HerokuBanner>;

export const Base: Story = () => <HerokuBanner />;
