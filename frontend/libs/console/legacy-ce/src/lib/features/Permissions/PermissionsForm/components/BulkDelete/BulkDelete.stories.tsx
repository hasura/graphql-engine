import { StoryObj, Meta } from '@storybook/react-webpack5';
import { ReactQueryDecorator } from '@hasura/shared/testing';
import { BulkDelete, BulkDeleteProps } from './index';

export default {
  component: BulkDelete,
  decorators: [ReactQueryDecorator()],
} as Meta;

export const Primary: StoryObj<BulkDeleteProps> = {
  render: (args) => {
    return <BulkDelete {...args} />;
  },

  args: {
    dataSourceName: 'default',
    roles: ['user'],
    handleClose: () => {},
  },
};
