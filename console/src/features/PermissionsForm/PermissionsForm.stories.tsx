import React from 'react';
import { Story, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';

import { PermissionsForm, PermissionsFormProps } from './PermissionsForm';
import { handlers } from './mocks/handlers.mock';

export default {
  title: 'Features/Permissions Tab/Permissions Form/Form',
  component: PermissionsForm,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta;

const roleName = 'user';

export const Insert: Story<PermissionsFormProps> = args => (
  <PermissionsForm {...args} />
);
Insert.args = {
  dataSourceName: 'default',

  queryType: 'insert',
  table: {
    schema: 'public',
    name: 'user',
  },
  roleName,
  handleClose: () => {},
};

export const Select: Story<PermissionsFormProps> = args => (
  <PermissionsForm {...args} />
);
Select.args = {
  ...Insert.args,
  queryType: 'select',
};

export const GDCSelect: Story<PermissionsFormProps> = args => (
  <PermissionsForm {...args} />
);
GDCSelect.args = {
  dataSourceName: 'sqlite',
  queryType: 'select',
  table: ['Artist'],
  roleName,
  handleClose: () => {},
};

export const Update: Story<PermissionsFormProps> = args => (
  <PermissionsForm {...args} />
);
Update.args = {
  ...Insert.args,
  queryType: 'update',
};

export const Delete: Story<PermissionsFormProps> = args => (
  <PermissionsForm {...args} />
);
Delete.args = {
  ...Insert.args,
  queryType: 'delete',
};
