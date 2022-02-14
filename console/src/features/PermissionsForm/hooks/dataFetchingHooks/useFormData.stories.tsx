import React from 'react';
import ReactJson from 'react-json-view';
import { Story, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';

import { useFormData, UseFormDataArgs } from './useFormData';
import { handlers } from '../../mocks/handlers.mock';

const UseFormDataComponent = ({
  schemaName,
  tableName,
  roleName,
  queryType,
}: UseFormDataArgs) => {
  const results = useFormData({ schemaName, tableName, roleName, queryType });
  return <ReactJson src={results} />;
};

export default {
  title: 'Permissions Form/hooks/useFormData',
  component: UseFormDataComponent,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers,
  },
} as Meta;

const schemaName = 'public';
const tableName = 'users';
const roleName = 'two';

export const Primary: Story<UseFormDataArgs> = args => (
  <UseFormDataComponent {...args} />
);
Primary.args = {
  schemaName,
  tableName,
  roleName,
  queryType: 'insert',
};
