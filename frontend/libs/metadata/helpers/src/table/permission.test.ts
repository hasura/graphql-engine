import { MetadataTable } from '@hasura/shared/types';
import { flattenTablePermissions, isDataQueryType } from './permission';

const metadataTable: MetadataTable = {
  table: { schema: 'public', name: 'users' },
  event_triggers: [],
  select_permissions: [
    { role: 'user', permission: { columns: '*' } },
    { role: 'manager', permission: { columns: ['id'] } },
  ],
  insert_permissions: [{ role: 'user', permission: { check: {} } }],
  update_permissions: [
    { role: 'manager', permission: { columns: '*', filter: {} } },
  ],
  delete_permissions: [{ role: 'admin', permission: { filter: {} } }],
};

describe('isDataQueryType', () => {
  it.each`
    input        | expected
    ${'insert'}  | ${true}
    ${'select'}  | ${true}
    ${'update'}  | ${true}
    ${'delete'}  | ${true}
    ${'upsert'}  | ${false}
    ${''}        | ${false}
    ${123}       | ${false}
    ${null}      | ${false}
    ${undefined} | ${false}
  `('returns $expected for $input', ({ input, expected }) => {
    expect(isDataQueryType(input)).toBe(expected);
  });
});

describe('flattenTablePermissions', () => {
  it('returns an empty array for null/undefined tables', () => {
    expect(flattenTablePermissions(null)).toEqual([]);
    expect(flattenTablePermissions(undefined)).toEqual([]);
  });

  it('flattens all four permission arrays into a single tagged list', () => {
    const flattened = flattenTablePermissions(metadataTable);
    expect(flattened).toHaveLength(5);
    // order is select -> insert -> update -> delete
    expect(flattened.map((p) => p.type)).toEqual([
      'select',
      'select',
      'insert',
      'update',
      'delete',
    ]);
  });

  it('tags each permission with its type and keeps the definition', () => {
    const flattened = flattenTablePermissions(metadataTable);
    const insert = flattened.find((p) => p.type === 'insert');
    expect(insert?.definition).toEqual({
      role: 'user',
      permission: { check: {} },
    });
  });

  it('omits permission kinds that are absent', () => {
    const flattened = flattenTablePermissions({
      table: { schema: 'public', name: 'users' },
      event_triggers: [],
      select_permissions: [{ role: 'user', permission: { columns: '*' } }],
    });
    expect(flattened).toHaveLength(1);
    expect(flattened[0].type).toBe('select');
  });
});
