import React from 'react';
import { rest } from 'msw';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ComponentMeta } from '@storybook/react';
import { NormalizedTable } from '@/dataSources/types';

import { DatabaseRelationshipsTab } from './DatabaseRelationshipsTab';
import { metadata } from '../RelationshipsTable/DatabaseRelationshipsTable/mocks';

const url = 'http://localhost:8080';

export default {
  title: 'Data Relationships/Database Relationships Tab',
  component: DatabaseRelationshipsTab,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: [
      rest.post(`${url}/v1/metadata`, (_req, res, ctx) =>
        res(ctx.json(metadata))
      ),
    ],
  },
} as ComponentMeta<typeof DatabaseRelationshipsTab>;

const tableSchema: NormalizedTable = {
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
  <DatabaseRelationshipsTab tableSchema={tableSchema} currentSource="default" />
);
