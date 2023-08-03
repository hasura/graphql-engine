import React from 'react';
import { StoryObj, Meta } from '@storybook/react';

import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';

import { BulkDelete, BulkDeleteProps } from './BulkDelete';

export default {
  component: BulkDelete,
  decorators: [ReactQueryDecorator()],
} as Meta;

export const Primary: StoryObj<BulkDeleteProps> = {
  render: args => {
    return <BulkDelete {...args} />;
  },

  args: {
    dataSourceName: 'default',
    roles: ['user'],
    handleClose: () => {},
  },
};
