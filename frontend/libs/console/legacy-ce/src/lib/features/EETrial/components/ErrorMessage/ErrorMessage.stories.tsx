import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ErrorMessage } from './ErrorMessage';

export default {
  title: 'features / EETrial / EE Error Message 🧬️',
  component: ErrorMessage,
} as ComponentMeta<typeof ErrorMessage>;

export const Default: ComponentStory<typeof ErrorMessage> = () => {
  return <ErrorMessage message={<div>Some error occured</div>} />;
};
Default.storyName = '💠 Default';
