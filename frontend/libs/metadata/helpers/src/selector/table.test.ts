import { MetadataTable } from '@hasura/shared/types';
import { findMetadataTableCoarse } from './table';

const tables: MetadataTable[] = [
  { table: { schema: 'public', name: 'Album' }, event_triggers: [] },
  { table: ['analytics', 'events'], event_triggers: [] },
];

describe('findMetadataTableCoarse', () => {
  it('matches a schema table by extracted schema + name', () => {
    expect(
      findMetadataTableCoarse(tables, { schema: 'public', name: 'Album' }),
    ).toBe(tables[0]);
  });

  it('matches across shapes (GDC array against schema/name query)', () => {
    // findMetadataTableCoarse normalizes via extractTableInfo, so it matches
    // a GDC array table using a plain { schema, name } query.
    expect(
      findMetadataTableCoarse(tables, { schema: 'analytics', name: 'events' }),
    ).toBe(tables[1]);
  });

  it('returns undefined when there is no match', () => {
    expect(
      findMetadataTableCoarse(tables, { schema: 'public', name: 'Artist' }),
    ).toBeUndefined();
  });

  it('returns undefined when tables is empty or undefined', () => {
    expect(
      findMetadataTableCoarse([], { schema: 'public', name: 'Album' }),
    ).toBeUndefined();
    expect(
      findMetadataTableCoarse(undefined, { schema: 'public', name: 'Album' }),
    ).toBeUndefined();
  });

  it('returns undefined when schema or name is falsy', () => {
    expect(findMetadataTableCoarse(tables, { name: 'Album' })).toBeUndefined();
    expect(
      findMetadataTableCoarse(tables, { schema: 'public' }),
    ).toBeUndefined();
  });
});
