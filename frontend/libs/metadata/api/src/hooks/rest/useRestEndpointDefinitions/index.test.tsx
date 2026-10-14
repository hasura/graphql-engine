import { beforeEach, describe, expect, it, vi } from 'vitest';
import { renderHook } from '@testing-library/react';
import { buildClientSchema, IntrospectionQuery } from 'graphql';
import introspectionNoCustom from './mocks/introspectionWithoutCustomizations.json';

const schema = buildClientSchema(
  introspectionNoCustom.data as unknown as IntrospectionQuery,
);

let introspectResult: { data: any; isLoading: boolean; error: any };
let metadataData: any;

vi.mock('../../graphql', () => ({
  __esModule: true,
  useIntrospectSchema: () => introspectResult,
}));

vi.mock('../../metadata', () => ({
  __esModule: true,
  useMetadata: () => ({ data: metadataData }),
}));

import { useRestEndpointDefinitions } from './index';

const source = {
  name: 'default',
  kind: 'postgres',
  tables: [
    {
      table: { name: 'user', schema: 'public' },
      configuration: {},
    },
  ],
  customization: {},
};

const userTable = { name: 'user', schema: 'public' };

beforeEach(() => {
  introspectResult = { data: schema, isLoading: false, error: undefined };
  metadataData = { restEndpoints: [], sources: [source] };
});

describe('useRestEndpointDefinitions (derived data)', () => {
  it('returns undefined data while the introspection schema is not ready', () => {
    introspectResult = { data: undefined, isLoading: true, error: undefined };
    const { result } = renderHook(() =>
      useRestEndpointDefinitions({
        dataSourceName: 'default',
        table: userTable,
      }),
    );
    expect(result.current.data).toBeUndefined();
    expect(result.current.isLoading).toBe(true);
  });

  it('derives endpoint definitions for the requested table on first render', () => {
    const { result } = renderHook(() =>
      useRestEndpointDefinitions({
        dataSourceName: 'default',
        table: userTable,
      }),
    );
    expect(result.current.data).toBeDefined();
    expect(result.current.data?.user).toBeDefined();
    // READ (select_by_pk) endpoint should be present for the user table
    expect(result.current.data?.user?.READ).toBeDefined();
  });

  it('returns undefined when the data source does not exist', () => {
    const { result } = renderHook(() =>
      useRestEndpointDefinitions({
        dataSourceName: 'does-not-exist',
        table: userTable,
      }),
    );
    expect(result.current.data).toBeUndefined();
  });

  it('recomputes when the table prop changes (previously an ignored dep)', () => {
    const { result, rerender } = renderHook(
      ({ table }) =>
        useRestEndpointDefinitions({ dataSourceName: 'default', table }),
      { initialProps: { table: userTable } },
    );
    expect(result.current.data?.user).toBeDefined();

    rerender({ table: { name: 'missing', schema: 'public' } });
    expect(result.current.data).toBeUndefined();
  });

  it('keeps a stable data reference across renders with unchanged inputs', () => {
    const { result, rerender } = renderHook(() =>
      useRestEndpointDefinitions({
        dataSourceName: 'default',
        table: userTable,
      }),
    );
    const first = result.current.data;
    rerender();
    expect(result.current.data).toBe(first);
  });
});
