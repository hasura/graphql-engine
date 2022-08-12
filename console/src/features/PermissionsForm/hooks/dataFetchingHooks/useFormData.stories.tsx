import React from 'react';
import ReactJson from 'react-json-view';
import { Story, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';

import { useFormData, UseFormDataArgs } from './useFormData';
import { handlers } from '../../mocks/handlers.mock';

const dataLeaf = {
  type: 'schema',
  name: 'users',
  leaf: {
    type: 'table',
    name: 'users',
  },
};

const UseFormDataComponent = ({
  dataTarget,
  roleName,
  queryType,
}: UseFormDataArgs) => {
  const results = useFormData({ dataTarget, roleName, queryType });
  return <ReactJson src={results} />;
};

export default {
  title: 'Features/Permissions Form/hooks/useFormData',
  component: UseFormDataComponent,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta;

const roleName = 'user';

export const Primary: Story<UseFormDataArgs> = args => (
  <UseFormDataComponent {...args} />
);
Primary.args = {
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
