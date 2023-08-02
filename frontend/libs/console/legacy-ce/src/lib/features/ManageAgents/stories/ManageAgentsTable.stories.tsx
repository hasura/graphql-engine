import React from 'react';
import { StoryFn, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { ManageAgentsTable } from '../components/ManageAgentsTable';
import { handlers } from '../mocks/handler.mock';

export default {
  title: 'Data/Agents/ManageAgentsTable',
  component: ManageAgentsTable,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta<typeof ManageAgentsTable>;

export const Primary: StoryFn<typeof ManageAgentsTable> = () => (
  <ManageAgentsTable />
);
