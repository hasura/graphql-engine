import React from 'react';
import { rest } from 'msw';

import { Meta, Story } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import {
  DatabaseRelationshipsTable,
  DatabaseRelationshipsTableProps,
} from './DatabaseRelationshipTable';

import { metadata, relationshipQueryResponse } from './mocks';

const url = 'http://localhost:8080';

export default {
  title: 'Features/Relationships/Database Relationship Table',
  component: DatabaseRelationshipsTable,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: [
      rest.post(`${url}/v1/metadata`, (_req, res, ctx) =>
        res(ctx.json(metadata))
      ),
      rest.post(`${url}/v1/query`, (_req, res, ctx) =>
        res(ctx.json(relationshipQueryResponse))
      ),
    ],
  },
} as Meta;

export const Primary: Story<DatabaseRelationshipsTableProps> = args => (
  <DatabaseRelationshipsTable {...args} />
);

Primary.args = {
  target: { database: 'default', table: 'Album', schema: 'public' },
};
