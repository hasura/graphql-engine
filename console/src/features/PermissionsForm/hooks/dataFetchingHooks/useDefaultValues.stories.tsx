import React from 'react';
import ReactJson from 'react-json-view';
import { Story, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';

import { useDefaultValues, UseDefaultValuesArgs } from './useDefaultValues';
import { handlers } from '../../mocks/handlers.mock';

const UseDefaultValuesComponent = ({
  schemaName,
  tableName,
  roleName,
  queryType,
}: UseDefaultValuesArgs) => {
  const results = useDefaultValues({
    schemaName,
    tableName,
    roleName,
    queryType,
  });
  return <ReactJson src={results} />;
};

export default {
  title: 'Features/Permissions Form/hooks/useDefaultValues',
  component: UseDefaultValuesComponent,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers,
    chromatic: { disableSnapshot: true },
  },
} as Meta;

const schemaName = 'public';
const tableName = 'users';
const roleName = 'user';

export const Insert: Story<UseDefaultValuesArgs> = args => (
  <UseDefaultValuesComponent {...args} />
);
Insert.args = {
  schemaName,
  tableName,
  roleName,
  queryType: 'insert',
};

export const Select: Story<UseDefaultValuesArgs> = args => (
  <UseDefaultValuesComponent {...args} />
);
Select.args = {
  ...Insert.args,
  queryType: 'select',
};

export const Update: Story<UseDefaultValuesArgs> = args => (
  <UseDefaultValuesComponent {...args} />
);
Update.args = {
  ...Insert.args,
  queryType: 'update',
};

export const Delete: Story<UseDefaultValuesArgs> = args => (
  <UseDefaultValuesComponent {...args} />
);
Delete.args = {
  ...Insert.args,
  queryType: 'delete',
};
