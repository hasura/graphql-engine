import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { ErrorMessage } from './ErrorMessage';

export default {
  title: 'features / EETrial / EE Error Message 🧬️',
  component: ErrorMessage,
} as Meta<typeof ErrorMessage>;

export const Default: StoryObj<typeof ErrorMessage> = {
  render: () => {
    return <ErrorMessage message={<div>Some error occured</div>} />;
  },

  name: '💠 Default',
};
