import React from 'react';
import { Story, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';

import { PermissionsTable, PermissionsTableProps } from './PermissionsTable';
import { handlers } from '../PermissionsForm/mocks/handlers.mock';
import { useTableMachine } from './hooks';

export default {
  title: 'Features/Permissions Table/Table',
  component: PermissionsTable,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta;

export const Default: Story<PermissionsTableProps> = args => {
  const machine = useTableMachine();

  return <PermissionsTable {...args} machine={machine} />;
};

Default.args = {
  dataSourceName: 'default',
  table: {
    schema: 'public',
    name: 'user',
  },
};

export const GDCTable: Story<PermissionsTableProps> = args => {
  const machine = useTableMachine();

  return <PermissionsTable {...args} machine={machine} />;
};

GDCTable.args = {
  dataSourceName: 'sqlite',
  table: ['Artist'],
};
