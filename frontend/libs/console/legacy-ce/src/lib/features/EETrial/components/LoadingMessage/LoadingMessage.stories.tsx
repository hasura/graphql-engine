import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { LoadingMessage } from './LoadingMessage';

export default {
  title: 'features / EETrial / EE Loading Message 🧬️',
  component: LoadingMessage,
} as Meta<typeof LoadingMessage>;

export const Default: StoryObj<typeof LoadingMessage> = {
  render: () => {
    return <LoadingMessage message="Loading your EE trial information..." />;
  },

  name: '💠 Default',
};
