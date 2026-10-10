import React from 'react';
import { StoryFn, Meta } from '@storybook/react-webpack5';
import { ReactQueryDecorator } from '@hasura/shared/testing';
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
