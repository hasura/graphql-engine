import React from 'react';
import { ReactQueryDecorator } from '../../storybook/decorators/react-query';
import { Meta } from '@storybook/react';
import { NormalizedTable } from '../../dataSources/types';
import { DatabaseRelationshipsTab } from './DatabaseRelationshipsTab_Legacy';

export default {
  title: 'Features/Data Relationships/Database Relationships Tab',
  component: DatabaseRelationshipsTab,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof DatabaseRelationshipsTab>;

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
    metadataTable={{ name: 'user', schema: 'public' }}
    currentSource="default"
    migrationMode
    driver="postgres"
  />
);
