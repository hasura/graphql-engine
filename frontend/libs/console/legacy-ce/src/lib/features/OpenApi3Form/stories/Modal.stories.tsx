import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ActionInitialModal } from '../components/Modal';

export default {
  title: 'Features/Actions/CreateModal',
  component: ActionInitialModal,
  decorators: [ReactQueryDecorator()],
  argTypes: {
    handleActionForm: { action: 'clicked' },
    handleOpenApiForm: { action: 'clicked' },
  },
} as ComponentMeta<typeof ActionInitialModal>;

export const Primary: ComponentStory<typeof ActionInitialModal> = args => (
  <ActionInitialModal
    handleActionForm={args.handleActionForm}
    handleOpenApiForm={args.handleOpenApiForm}
  />
);
