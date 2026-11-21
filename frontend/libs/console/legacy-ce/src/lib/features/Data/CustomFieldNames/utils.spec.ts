import {
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

      update_many: 'update_customizeTableName_many (default)',
    };

    const result = customFieldNamesPlaceholders('customizeTableName');

    expect(result).toEqual(expected);
  });
});

describe('customFieldNamesPlaceholders with the graphql-default naming convention', () => {
  it('camel-cases the names generated from the table name', () => {
    const result = customFieldNamesPlaceholders('app_users', {
      namingConvention: 'graphql-default',
    });

    expect(result).toEqual({
      custom_name: 'appUsers (default)',
      select: 'appUsers (default)',
      select_by_pk: 'appUsersByPk (default)',
      select_aggregate: 'appUsersAggregate (default)',
      select_stream: 'appUsersStream (default)',
      insert: 'insertAppUsers (default)',
      insert_one: 'insertAppUsersOne (default)',
      update: 'updateAppUsers (default)',
      update_by_pk: 'updateAppUsersByPk (default)',
      delete: 'deleteAppUsers (default)',
      delete_by_pk: 'deleteAppUsersByPk (default)',
      update_many: 'updateAppUsersMany (default)',
      logical_model: 'Select a logical model',
    });
  });

  it('keeps a custom table name as it is', () => {
    const result = customFieldNamesPlaceholders('my_table', {
      namingConvention: 'graphql-default',
      isCustomName: true,
    });

    expect(result.select).toBe('my_table (default)');
    expect(result.select_by_pk).toBe('my_tableByPk (default)');
    expect(result.select_aggregate).toBe('my_tableAggregate (default)');
    expect(result.insert).toBe('insertMy_table (default)');
    expect(result.update_many).toBe('updateMy_tableMany (default)');
  });

  it('keeps a leading underscore of the table name', () => {
    const result = customFieldNamesPlaceholders('_audit_log', {
      namingConvention: 'graphql-default',
    });

    expect(result.select).toBe('_auditLog (default)');
    expect(result.delete_by_pk).toBe('delete_auditLogByPk (default)');
  });

  it('uses snake case for hasura-default', () => {
    const result = customFieldNamesPlaceholders('app_users', {
      namingConvention: 'hasura-default',
    });

    expect(result.update_by_pk).toBe('update_app_users_by_pk (default)');
    expect(result.update_many).toBe('update_app_users_many (default)');
  });
});
