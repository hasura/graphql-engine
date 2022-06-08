import React from 'react';
import { rest } from 'msw';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ComponentMeta } from '@storybook/react';
import { NormalizedTable } from '@/dataSources/types';

import { DatabaseRelationshipsTab } from './DatabaseRelationshipsTab';
import {
  metadata,
  relationshipQueryResponse,
} from '../RelationshipsTable/DatabaseRelationshipsTable/mocks';

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
      rest.post(`${url}/v2/query`, (_req, res, ctx) =>
        res(ctx.json(relationshipQueryResponse))
      ),
    ],
  },
} as ComponentMeta<typeof DatabaseRelationshipsTab>;

const tableSchema: NormalizedTable = {
  table_schema: 'public',
  table_name: 'Album',
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
