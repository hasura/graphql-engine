import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';

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
  render: args => {
    const machine = useTableMachine();

    return <PermissionsTable {...args} machine={machine} />;
  },

  args: {
    dataSourceName: 'Lite',
    table: ['Artist'],
  },
};
