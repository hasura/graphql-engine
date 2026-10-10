import { TableFunction } from '@hasura/shared/types';
import {
  adaptFunction,
  areFunctionsEqual,
  areFunctionsEqualCoalesce,
  functionDisplayName,
  search,
} from './function';
import { areTablesEqual, areTablesEqualCoalesce } from '../table/predicate';

describe('adaptFunction', () => {
  it('defaults schema to "public" for a single-element array', () => {
    expect(adaptFunction(['my_function'])).toEqual({
      schema: 'public',
      name: 'my_function',
    });
  });

  it('uses [schema, name] for a two-element array', () => {
    expect(adaptFunction(['custom_schema', 'my_function'])).toEqual({
      schema: 'custom_schema',
      name: 'my_function',
    });
  });

  it('defaults schema to "public" for a bare string', () => {
    expect(adaptFunction('my_function' as unknown as TableFunction)).toEqual({
      schema: 'public',
      name: 'my_function',
    });
  });

  it('passes through an object with schema/name', () => {
    expect(
      adaptFunction({
        schema: 'custom_schema',
        name: 'my_function',
      } as unknown as TableFunction),
    ).toEqual({ schema: 'custom_schema', name: 'my_function' });
  });
});

describe('search', () => {
  const functions = [
    { qualifiedFunction: ['public', 'search_albums'] as TableFunction },
    { qualifiedFunction: ['custom', 'find_artists'] as TableFunction },
    { qualifiedFunction: ['my_function'] as TableFunction },
  ];

  it('returns all functions when the search text is empty', () => {
    expect(search(functions, '')).toBe(functions);
  });

  it('filters by "schema / name" case-insensitively', () => {
    expect(search(functions, 'ALBUMS')).toEqual([functions[0]]);
  });

  it('matches on the schema portion too', () => {
    expect(search(functions, 'custom')).toEqual([functions[1]]);
  });

  it('matches the "public" default schema of single-element functions', () => {
    expect(search(functions, 'public / my_function')).toEqual([functions[2]]);
  });

  it('returns an empty array when nothing matches', () => {
    expect(search(functions, 'nonexistent')).toEqual([]);
  });
});

describe('functionDisplayName', () => {
  it('joins schema and name with the default separator', () => {
    expect(
      functionDisplayName({ qualifiedFunction: ['public', 'my_function'] }),
    ).toBe('public / my_function');
  });

  it('prefixes the data source name when provided', () => {
    expect(
      functionDisplayName({
        qualifiedFunction: ['public', 'my_function'],
        dataSourceName: 'chinook',
      }),
    ).toBe('chinook / public / my_function');
  });

  it('honours a custom separator', () => {
    expect(
      functionDisplayName({
        qualifiedFunction: ['public', 'my_function'],
        dataSourceName: 'chinook',
        separator: '.',
      }),
    ).toBe('chinook.public.my_function');
  });

  it('uses the "public" default for single-element functions', () => {
    expect(functionDisplayName({ qualifiedFunction: ['my_function'] })).toBe(
      'public / my_function',
    );
  });
});

describe('areFunctionsEqual / areFunctionsEqualCoalesce aliases', () => {
  it('areFunctionsEqual is the same reference as areTablesEqual', () => {
    expect(areFunctionsEqual).toBe(areTablesEqual);
  });

  it('areFunctionsEqualCoalesce is the same reference as areTablesEqualCoalesce', () => {
    expect(areFunctionsEqualCoalesce).toBe(areTablesEqualCoalesce);
  });

  it('areFunctionsEqual does raw structural comparison', () => {
    expect(areFunctionsEqual(['public', 'f'], ['public', 'f'])).toBe(true);
    expect(areFunctionsEqual(['public', 'f'], ['public', 'g'])).toBe(false);
  });

  it('areFunctionsEqualCoalesce normalizes shapes before comparing', () => {
    expect(
      areFunctionsEqualCoalesce(['public', 'f'], {
        schema: 'public',
        name: 'f',
      }),
    ).toBe(true);
  });
});
