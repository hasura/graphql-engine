import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ManageAgentsTable } from '../components/ManageAgentsTable';
import { handlers } from '../mocks/handler.mock';

export default {
  title: 'Data/Agents/ManageAgentsTable',
  component: ManageAgentsTable,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as ComponentMeta<typeof ManageAgentsTable>;

export const Primary: ComponentStory<typeof ManageAgentsTable> = () => (
  <ManageAgentsTable />
);
