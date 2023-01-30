import React from 'react';
import { Story, Meta } from '@storybook/react';

import { ReactQueryDecorator } from '@/storybook/decorators/react-query';

import { BulkDelete, BulkDeleteProps } from './BulkDelete';

import { handlers } from './mocks/handlers.mock';

export default {
  title: 'Features/Permissions/Bulk Update',
  component: BulkDelete,
  decorators: [ReactQueryDecorator()],
} as Meta;

export const Primary: Story<BulkDeleteProps> = args => {
  return <BulkDelete {...args} />;
};
Primary.args = {
  dataSourceName: 'default',
  roles: ['user'],
  handleClose: () => {},
};
Primary.parameters = {
  msw: handlers(),
};
