import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import {
  CustomFieldNamesModal,
  CustomFieldNamesModalProps,
} from './CustomFieldNamesModal';

export default {
  component: CustomFieldNamesModal,
  argTypes: {
    onSubmit: { action: true },
    onClose: { action: true },
  },
} as ComponentMeta<typeof CustomFieldNamesModal>;

export const Primary: Story<CustomFieldNamesModalProps> = args => (
  <CustomFieldNamesModal {...args} />
);

Primary.args = {
  tableName: 'Customer',
};
