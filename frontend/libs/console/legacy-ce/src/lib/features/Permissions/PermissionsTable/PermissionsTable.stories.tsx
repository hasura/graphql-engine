import React from 'react';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { ReactQueryDecorator } from '@hasura/shared/testing';

import { PermissionsTable, PermissionsTableProps } from './PermissionsTable';
import { handlers } from '../PermissionsForm/mocks/handlers.mock';
import { useTableMachine } from './hooks';

export default {
  component: PermissionsTable,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta;

// Fails
export const GDCTable: StoryObj<PermissionsTableProps> = {
  render: (args) => {
    const machine = useTableMachine();

    return <PermissionsTable {...args} machine={machine} />;
  },

  args: {
    source: {
      name: 'Lite',
      kind: 'sqlite',
    },
    table: ['Artist'],
  },
};
