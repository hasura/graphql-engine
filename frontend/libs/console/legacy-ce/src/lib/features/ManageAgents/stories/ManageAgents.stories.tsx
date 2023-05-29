import React from 'react';
import { StoryFn, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { ManageAgents } from '../components/ManageAgents';
import { handlers } from '../mocks/handler.mock';

export default {
  title: 'Data/Agents/ManageAgents',
  component: ManageAgents,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta<typeof ManageAgents>;

export const Primary: StoryFn<typeof ManageAgents> = () => <ManageAgents />;
