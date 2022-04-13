import { NormalizedTable } from '@/dataSources/types';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ComponentMeta } from '@storybook/react';
import React from 'react';
import { DatabaseRelationshipsTab } from './DatabaseRelationshipsTab';

export default {
  title: 'Relationships / Database Relationships Tab',
  component: DatabaseRelationshipsTab,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof DatabaseRelationshipsTab>;

const tableSchema: NormalizedTable = {
  table_schema: 'public',
  table_name: 'author',
  table_type: 'TABLE',
  is_table_tracked: true,
  columns: [],
  comment: null,
  triggers: [],
  view_info: null,
};

export const Main = () => (
  <DatabaseRelationshipsTab tableSchema={tableSchema} currentSource="default" />
);
