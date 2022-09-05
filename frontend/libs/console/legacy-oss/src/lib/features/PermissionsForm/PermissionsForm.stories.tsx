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
    msw: handlers,
  },
} as Meta;

const roleName = 'user';

const dataLeaf = {
  type: 'schema',
  name: 'users',
  leaf: {
    type: 'table',
    name: 'users',
  },
};

export const Showcase: Story<PermissionsFormProps> = () => {
  return (
    <>
      <p className="font-bold py-4">Query Type: Insert</p>

      <PermissionsForm
        dataLeaf={dataLeaf}
        roleName={roleName}
        accessType="partialAccess"
        queryType="insert"
        handleClose={() => {}}
      />

      <p className="font-bold py-4">Query Type: Select</p>

      <PermissionsForm
        dataLeaf={dataLeaf}
        roleName={roleName}
        accessType="partialAccess"
        queryType="select"
        handleClose={() => {}}
      />

      <p className="font-bold py-4">Query Type: Update</p>

      <PermissionsForm
        dataLeaf={dataLeaf}
        roleName={roleName}
        accessType="noAccess"
        queryType="update"
        handleClose={() => {}}
      />

      <p className="font-bold py-4">Query Type: Delete</p>

      <PermissionsForm
        dataLeaf={dataLeaf}
        roleName={roleName}
        accessType="noAccess"
        queryType="delete"
        handleClose={() => {}}
      />
    </>
  );
};

export const Insert: Story<PermissionsFormProps> = args => (
  <PermissionsForm {...args} />
);
Insert.args = {
  dataLeaf,
  roleName,
  accessType: 'partialAccess',
  queryType: 'insert',
  handleClose: () => {},
};
Insert.parameters = {
  // Disable storybook for Insert stories
  chromatic: { disableSnapshot: true },
};

export const Select: Story<PermissionsFormProps> = args => (
  <PermissionsForm {...args} />
);
Select.args = {
  ...Insert.args,
  queryType: 'select',
};
Select.parameters = Insert.parameters;

export const Update: Story<PermissionsFormProps> = args => (
  <PermissionsForm {...args} />
);
Update.args = {
  ...Insert.args,
  queryType: 'update',
  accessType: 'noAccess',
};
Update.parameters = Insert.parameters;

export const Delete: Story<PermissionsFormProps> = args => (
  <PermissionsForm {...args} />
);
Delete.args = {
  ...Insert.args,
  queryType: 'delete',
  accessType: 'noAccess',
};
Delete.parameters = Insert.parameters;
