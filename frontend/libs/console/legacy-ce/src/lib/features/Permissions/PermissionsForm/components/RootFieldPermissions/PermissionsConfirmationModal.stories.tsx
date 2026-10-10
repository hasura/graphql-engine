import React from 'react';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import {
  PermissionsConfirmationModal,
  Props,
} from './PermissionsConfirmationModal';

export default {
  component: PermissionsConfirmationModal,
  argTypes: {
    onSubmit: { action: true },
    onClose: { action: true },
  },
} as Meta<typeof PermissionsConfirmationModal>;

export const Base: StoryObj<Props> = {
  args: {
    title: <>title</>,
    description: <>description</>,
  },
};
