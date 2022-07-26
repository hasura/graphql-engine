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
} as Meta;

export const Default: Story<PermissionsTableProps> = args => {
  const machine = useTableMachine();

  return <PermissionsTable {...args} machine={machine} />;
};

Default.args = {
  dataLeaf: {
    type: 'schema',
    name: 'public',
    leaf: {
      type: 'table',
      name: 'users',
    },
  },
};

Default.parameters = {
  msw: handlers(),
};
