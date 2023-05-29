import { StoryObj, Meta } from '@storybook/react';
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
} as Meta<typeof CustomFieldNamesModal>;

export const Primary: StoryObj<CustomFieldNamesModalProps> = {
  args: {
    tableName: 'Customer',
  },
};
