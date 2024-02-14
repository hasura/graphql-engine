import {
  getTrackTableType,
  customFieldNamesPlaceholders,
  getQualifiedTableForCustomFieldNames,
} from './utils';

import { CustomFieldNamesFormVals } from './types';

describe('getTrackTableType', () => {
  it.each`
    driver        | expected
    ${'postgres'} | ${'pg_track_table'}
    ${'mssql'}    | ${'mssql_track_table'}
    ${'mysql'}    | ${'mysql_track_table'}
    ${'citus'}    | ${'citus_track_table'}
    ${'bigquery'} | ${'bigquery_track_table'}
  `(
    'Given a $driver driver, then returns $expected',
    ({ driver, expected }) => {
      expect(getTrackTableType(driver)).toBe(expected);
    }
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
        })
      ).toEqual(expected);
    }
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
