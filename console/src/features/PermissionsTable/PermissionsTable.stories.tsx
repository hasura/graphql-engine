import React from 'react';
import { QueryClient, QueryClientProvider } from 'react-query';
import { Story, Meta } from '@storybook/react';

import {
  TableForm,
  PermissionsTable,
  PermissionsTableProps,
} from './PermissionsTable';
import { handlers } from './mocks/handlers.mock';

const queryClient = new QueryClient();

export default {
  title: 'Permissions Table',
  component: PermissionsTable,
  decorators: [
    (StoryComponent: React.FC) => (
      <QueryClientProvider client={queryClient}>
        <TableForm>
          <StoryComponent />
        </TableForm>
      </QueryClientProvider>
    ),
  ],
} as Meta;

export const Default: Story<PermissionsTableProps> = args => (
  <PermissionsTable {...args} />
);
Default.args = {
  schemaName: 'public',
  tableName: 'users',
  selected: {
    queryType: 'insert',
    roleName: 'user',
    accessType: 'fullAccess',
  },
  onChange: () => {},
};
Default.parameters = {
  msw: handlers,
};
