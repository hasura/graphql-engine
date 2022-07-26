import React from 'react';
import ReactJson from 'react-json-view';
import { Story, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';

import { useDefaultValues, UseDefaultValuesArgs } from './useDefaultValues';
import { handlers } from '../../mocks/handlers.mock';

const dataLeaf = {
  type: 'schema',
  name: 'users',
  leaf: {
    type: 'table',
    name: 'users',
  },
};

const UseDefaultValuesComponent = ({
  dataTarget,
  roleName,
  queryType,
}: UseDefaultValuesArgs) => {
  const results = useDefaultValues({
    dataTarget,
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
    msw: handlers(),
    chromatic: { disableSnapshot: true },
  },
} as Meta;

const roleName = 'user';

export const Insert: Story<UseDefaultValuesArgs> = args => (
  <UseDefaultValuesComponent {...args} />
);
Insert.args = {
  dataTarget: {
    dataSource: {
      driver: 'postgres',
      database: 'default',
    },
    dataLeaf,
  },
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
