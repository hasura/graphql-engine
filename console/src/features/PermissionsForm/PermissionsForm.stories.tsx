import React from 'react';
import { Story, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';

import { PermissionsForm, PermissionsFormProps } from './PermissionsForm';
import { handlers } from './mocks/handlers.mock';

export default {
  title: 'Features/Permissions Form/Form',
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
  currentSource: 'postgres',
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
Select.parameters = Insert.parameters;

export const GDCSelect: Story<PermissionsFormProps> = args => (
  <PermissionsForm {...args} />
);
GDCSelect.args = {
  currentSource: 'sqlite',
  dataSourceName: 'sqlite',
  queryType: 'select',
  table: ['Artist'],
  roleName,
  handleClose: () => {},
};
GDCSelect.parameters = Insert.parameters;

export const Update: Story<PermissionsFormProps> = args => (
  <PermissionsForm {...args} />
);
Update.args = {
  ...Insert.args,
  queryType: 'update',
};
Update.parameters = Insert.parameters;

export const Delete: Story<PermissionsFormProps> = args => (
  <PermissionsForm {...args} />
);
Delete.args = {
  ...Insert.args,
  queryType: 'delete',
};
Delete.parameters = Insert.parameters;
