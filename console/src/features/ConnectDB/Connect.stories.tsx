// Button.stories.ts|tsx

import React from 'react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { Connect } from './Connect';

export default {
  /* 👇 The title prop is optional.
   * See https://storybook.js.org/docs/react/configure/overview#configure-story-loading
   * to learn how to generate automatic titles
   */
  title: 'Data/Connect',
  component: Connect,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof Connect>;

export const Primary: ComponentStory<typeof Connect> = () => <Connect />;
