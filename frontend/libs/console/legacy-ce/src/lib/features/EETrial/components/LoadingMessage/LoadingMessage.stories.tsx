import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { LoadingMessage } from './LoadingMessage';

export default {
  title: 'features / EETrial / EE Loading Message 🧬️',
  component: LoadingMessage,
} as ComponentMeta<typeof LoadingMessage>;

export const Default: ComponentStory<typeof LoadingMessage> = () => {
  return <LoadingMessage message="Loading your EE trial information..." />;
};
Default.storyName = '💠 Default';
