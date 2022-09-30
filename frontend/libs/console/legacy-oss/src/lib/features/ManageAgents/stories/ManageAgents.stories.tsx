import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ManageAgents } from '../components/ManageAgents';
import { handlers } from '../mocks/handler.mock';

export default {
  title: 'Data/Agents/ManageAgents',
  component: ManageAgents,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as ComponentMeta<typeof ManageAgents>;

export const Primary: ComponentStory<typeof ManageAgents> = () => (
  <ManageAgents />
);
