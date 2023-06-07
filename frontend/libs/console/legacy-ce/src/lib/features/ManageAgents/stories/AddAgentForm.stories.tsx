import React from 'react';
import { StoryFn, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { AddAgentForm } from '../components/AddAgentForm';
import { handlers } from '../mocks/handler.mock';

export default {
  title: 'Data/Agents/AddAgentForm',
  component: AddAgentForm,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta<typeof AddAgentForm>;

export const Primary: StoryFn<typeof AddAgentForm> = () => (
  <AddAgentForm onClose={() => {}} />
);
