import React from 'react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ComponentMeta } from '@storybook/react';
import { NormalizedTable } from '@/dataSources/types';

import { DatabaseRelationshipsTab } from './DatabaseRelationshipsTab';
import { handlers } from './__mocks__';

export default {
  title: 'Features/Data Relationships/Database Relationships Tab',
  component: DatabaseRelationshipsTab,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as ComponentMeta<typeof DatabaseRelationshipsTab>;

const table: NormalizedTable = {
  table_schema: 'public',
  table_name: 'user',
  table_type: 'TABLE',
  is_table_tracked: true,
  columns: [],
  comment: null,
  triggers: [],
  view_info: null,
};

export const Primary = () => (
  <DatabaseRelationshipsTab
    table={table}
    currentSource="default"
    migrationMode
    driver="postgres"
  />
);
