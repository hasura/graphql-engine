import { StoryObj, Meta } from '@storybook/react';
import {
  CustomFieldNamesModal,
  CustomFieldNamesModalProps,
} from './CustomFieldNamesModal';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';

export default {
  component: CustomFieldNamesModal,
  argTypes: {
    onSubmit: { action: true },
    onClose: { action: true },
  },
  decorators: [ReactQueryDecorator()],
} as Meta<typeof CustomFieldNamesModal>;

export const Primary: StoryObj<CustomFieldNamesModalProps> = {
  args: {
    tableName: 'Customer',
  },
};
