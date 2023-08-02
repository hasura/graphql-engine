import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { ActionInitialModal } from '../components/Modal';

export default {
  title: 'Features/Actions/CreateModal',
  component: ActionInitialModal,
  decorators: [ReactQueryDecorator()],
  argTypes: {
    handleActionForm: { action: 'clicked' },
    handleOpenApiForm: { action: 'clicked' },
  },
} as Meta<typeof ActionInitialModal>;

export const Primary: StoryObj<typeof ActionInitialModal> = {
  render: args => (
    <ActionInitialModal
      handleActionForm={args.handleActionForm}
      handleOpenApiForm={args.handleOpenApiForm}
    />
  ),
};
