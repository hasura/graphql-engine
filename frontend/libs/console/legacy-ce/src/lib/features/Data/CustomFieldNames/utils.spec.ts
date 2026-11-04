import {
  getOrderedCustomRootFields,
  getTrackTableType,
  customFieldNamesPlaceholders,
  getQualifiedTableForCustomFieldNames,
} from './utils';

import { CustomFieldNamesFormVals } from './types';

describe('getTrackTableType', () => {
  it.each`
    driver         | expected
    ${'postgres'}  | ${'pg_track_table'}
    ${'mssql'}     | ${'mssql_track_table'}
    ${'mysql'}     | ${'mysql_track_table'}
    ${'citus'}     | ${'citus_track_table'}
    ${'cockroach'} | ${'cockroach_track_table'}
    ${'bigquery'}  | ${'bigquery_track_table'}
  `(
    'Given a $driver driver, then returns $expected',
    ({ driver, expected }) => {
      expect(getTrackTableType(driver)).toBe(expected);
    },
  );
});

describe('getTableObjectType', () => {
  it.each`
    driver        | schema        | tableName  | expected
    ${'mysql'}    | ${'aSchema'}  | ${'table'} | ${{ name: 'table', schema: 'aSchema' }}
    ${'mssql'}    | ${'aSchema'}  | ${'table'} | ${{ name: 'table', schema: 'aSchema' }}
    ${'citus'}    | ${'aSchema'}  | ${'table'} | ${{ name: 'table', schema: 'aSchema' }}
    ${'postgres'} | ${'aSchema'}  | ${'table'} | ${{ name: 'table', schema: 'aSchema' }}
    ${'bigquery'} | ${'aDataset'} | ${'table'} | ${{ name: 'table', dataset: 'aDataset' }}
  `(
    'Given a $driver driver, a $schema schema, and a $tableName table name, then returns the qualified table $expected',
    ({ driver, schema, tableName, expected }) => {
      expect(
        getQualifiedTableForCustomFieldNames({
          driver,
          schema,
          tableName,
        }),
      ).toEqual(expected);
    },
  );
});

describe('getTrackingTableFormPlaceholders', () => {
  it('returns the placeholder', () => {
    const expected: CustomFieldNamesFormVals = {
      custom_name: 'customizeTableName (default)',

      select: 'customizeTableName (default)',
      select_by_pk: 'customizeTableName_by_pk (default)',
      select_aggregate: 'customizeTableName_aggregate (default)',
      select_stream: 'customizeTableName_stream (default)',

      insert: 'insert_customizeTableName (default)',
      insert_one: 'insert_customizeTableName_one (default)',
      logical_model: 'Select a logical model',

      update: 'update_customizeTableName (default)',
      update_by_pk: 'update_customizeTableName_by_pk (default)',

      delete: 'delete_customizeTableName (default)',
      delete_by_pk: 'delete_customizeTableName_by_pk (default)',

      update_many: 'update_many_customizeTableName (default)',
    };

    const result = customFieldNamesPlaceholders('customizeTableName');

    expect(result).toEqual(expected);
  });
});

describe('getOrderedCustomRootFields', () => {
  it('lists the root fields in the order of the editor', () => {
    // the order in which the server returns a configuration (#8253)
    const fromServer = {
      insert: 'i',
      select_aggregate: 'sa',
      insert_one: 'io',
      select_by_pk: 'sbp',
      select: 's',
      delete: 'd',
      update: 'u',
      delete_by_pk: 'dbp',
      update_by_pk: 'ubp',
    };
    expect(getOrderedCustomRootFields(fromServer).map(([key]) => key)).toEqual([
      'select',
      'select_by_pk',
      'select_aggregate',
      'insert',
      'insert_one',
      'update',
      'update_by_pk',
      'delete',
      'delete_by_pk',
    ]);
  });

  it('puts unknown root fields last and skips empty ones', () => {
    expect(
      getOrderedCustomRootFields({
        future_field: 'f',
        select: 's',
        insert: '',
      }),
    ).toEqual([
      ['select', 's'],
      ['future_field', 'f'],
    ]);
  });
});
