import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import {
  PermissionsConfirmationModal,
  Props,
} from './PermissionsConfirmationModal';

export default {
  title: 'Features/Permissions Form/Permissions Confirmation Modal',
  component: PermissionsConfirmationModal,
  argTypes: {
    onSubmit: { action: true },
    onClose: { action: true },
  },
} as ComponentMeta<typeof PermissionsConfirmationModal>;

export const Base: Story<Props> = args => (
  <PermissionsConfirmationModal {...args} />
);

Base.args = {
  title: <>title</>,
  description: <>description</>,
};
