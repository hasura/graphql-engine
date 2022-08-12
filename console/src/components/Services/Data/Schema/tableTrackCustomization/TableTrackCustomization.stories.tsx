import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import {
  TableTrackingCustomizationModal,
  TableTrackingCustomizationModalProps,
} from './TableTrackingCustomizationModal';

export default {
  title: 'data/TableTrackingCustomizationModal',
  component: TableTrackingCustomizationModal,
  argTypes: {
    onSubmit: { action: true },
    onClose: { action: true },
  },
} as ComponentMeta<typeof TableTrackingCustomizationModal>;

export const Primary: Story<TableTrackingCustomizationModalProps> = args => (
  <TableTrackingCustomizationModal {...args} />
);

Primary.args = {
  tableName: 'author',
};
