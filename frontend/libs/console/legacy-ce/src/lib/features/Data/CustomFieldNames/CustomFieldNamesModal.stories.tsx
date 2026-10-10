import { StoryObj, Meta } from '@storybook/react-webpack5';
import {
  CustomFieldNamesModal,
  CustomFieldNamesModalProps,
} from './CustomFieldNamesModal';
import { ReactQueryDecorator } from '@hasura/shared/testing';

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
