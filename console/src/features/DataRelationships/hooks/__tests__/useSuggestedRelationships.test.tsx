import React from 'react';
import { renderHook } from '@testing-library/react-hooks';

import { rest } from 'msw';
import { setupServer } from 'msw/node';
import { QueryClient, QueryClientProvider } from 'react-query';
import { Provider } from 'react-redux';
import { store } from '../../../../store';

import {
  ExistingRelationships,
  ForeignKeyConstraints,
  removeExistingRelationships,
  useSuggestedRelationships,
} from '../useSuggestedRelationships';
import { metadata, query } from '../mocks/dataStubs';

const target = {
  database: 'default',
  schema: 'public',
  table: 'user',
};

const fk: ForeignKeyConstraints = [
  {
    from: {
      table: 'product',
      column: ['fk_user_id'],
    },
    to: {
      table: 'user',
      column: ['id'],
    },
  },
];

const existing: ExistingRelationships = [
  {
    fromType: 'table',
    toType: 'table',
    name: 'user',
    reference: 'default',
    referenceTable: 'product',
    target: 'default',
    targetTable: 'user',
    type: 'Object',
    fieldsFrom: ['fk_user_id'],
    fieldsTo: ['id'],
    relationship: {
      name: 'user',
      using: {
        manual_configuration: {
          remote_table: {
            schema: 'public',
            name: 'user',
          },

          column_mapping: {
            fk_user_id: 'id',
          },
        },
      },
    },
  },
];

describe('remove existing relationships function', () => {
  it('if no existing relationships returns original values', () => {
    const res = removeExistingRelationships({
      target,
      foreignKeyConstraints: fk,
      allExistingRelationships: [],
    });

    expect(res?.[0].from).toEqual({
      column: ['id'],
      table: 'user',
    });

    expect(res?.[0].to).toEqual({
      column: ['fk_user_id'],
      table: 'product',
    });

    expect(res?.[0].type).toEqual('array');
  });

  it('if existing relationships exist they are removed', () => {
    const res = removeExistingRelationships({
      target,
      foreignKeyConstraints: fk,
      allExistingRelationships: existing,
    });

    expect(res).toEqual([]);
  });
});

const server = setupServer(
  rest.post(`http://localhost/v1/metadata`, (_req, res, ctx) => {
    return res(ctx.json(metadata));
  }),
  rest.post('http://localhost/v2/query', (_req, res, ctx) => {
    return res(ctx.status(200), ctx.json(query));
  })
);

const queryClient = new QueryClient();
const wrapper = ({ children }: { children: React.ReactNode }) => (
  <Provider store={store}>
    <QueryClientProvider client={queryClient}>{children}</QueryClientProvider>
  </Provider>
);

describe('useSuggestedRelationships hook', () => {
  beforeAll(() => server.listen());
  afterAll(() => server.close());

  it('should return a list of existing relationships', async () => {
    const { result, waitFor } = renderHook(
      () => useSuggestedRelationships(target),
      {
        wrapper,
      }
    );

    await waitFor(() => result.current.isSuccess);

    const res = result.current.data;

    expect(res?.[0].from).toEqual({
      column: ['id'],
      table: 'user',
    });

    expect(res?.[0].to).toEqual({
      column: ['fk_user_id'],
      table: 'product',
    });

    expect(res?.[0].type).toEqual('array');
  });
});
