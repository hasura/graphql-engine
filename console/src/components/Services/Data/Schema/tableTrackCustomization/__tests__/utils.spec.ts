import { Driver } from '../../../../../../dataSources/index';
import {
  getDriverPrefix,
  getQualifiedTable,
  GetTablePayloadArgs,
  getTrackingTableFormPlaceholders,
  getTrackTableType,
  QualifiedTable,
} from '../utils';

describe('getTrackingTableFormPlaceholders', () => {
  it('returns the placeholder', () => {
    const result = getTrackingTableFormPlaceholders('customizeTableName');
    expect(result).toEqual({
      custom_name: 'customizeTableName (default)',
      select: 'customizeTableName (default)',
      select_by_pk: 'customizeTableName_by_pk (default)',
      select_aggregate: 'customizeTableName_aggregate (default)',
      insert: 'insert_customizeTableName (default)',
      insert_one: 'insert_customizeTableName_one (default)',
      update: 'update_customizeTableName (default)',
      update_by_pk: 'update_by_pk_customizeTableName (default)',
      delete: 'delete_customizeTableName (default)',
      delete_by_pk: 'delete_by_pk_customizeTableName (default)',
    });
  });
});

describe('getDriverPrefix', () => {
  describe('when driver is postgres', () => {
    it('returns pg', () => {
      expect(getDriverPrefix('postgres')).toBe('pg');
    });
  });
  describe('when driver is not postgres', () => {
    const tests: { driver: Driver; expected: 'pg' | Driver }[] = [
      {
        driver: 'mssql',
        expected: 'mssql',
      },
      {
        driver: 'mysql',
        expected: 'mysql',
      },
      {
        driver: 'bigquery',
        expected: 'bigquery',
      },
      {
        driver: 'citus',
        expected: 'citus',
      },
    ];
    it.each(tests)('returns $expected from $driver', ({ driver, expected }) => {
      expect(getDriverPrefix(driver)).toBe(expected);
    });
  });
});

describe('getTrackTableType', () => {
  const tests: { driver: Driver; expected: string }[] = [
    {
      driver: 'postgres',
      expected: 'pg_track_table',
    },
    {
      driver: 'mssql',
      expected: 'mssql_track_table',
    },
    {
      driver: 'mysql',
      expected: 'mysql_track_table',
    },
    {
      driver: 'bigquery',
      expected: 'bigquery_track_table',
    },
    {
      driver: 'citus',
      expected: 'citus_track_table',
    },
  ];
  it.each(tests)('returns $expected from $driver', ({ driver, expected }) => {
    expect(getTrackTableType(driver)).toBe(expected);
  });
});

interface QualifiedTableTest extends GetTablePayloadArgs {
  expected: QualifiedTable;
}

describe('getTableObjectType', () => {
  const tests: QualifiedTableTest[] = [
    {
      driver: 'postgres',
      schema: 'aSchema',
      tableName: 'aTableName',
      expected: { name: 'aTableName', schema: 'aSchema' },
    },
    {
      driver: 'mysql',
      schema: 'aSchema',
      tableName: 'aTableName',
      expected: { name: 'aTableName', schema: 'aSchema' },
    },
    {
      driver: 'mssql',
      schema: 'aSchema',
      tableName: 'aTableName',
      expected: { name: 'aTableName', schema: 'aSchema' },
    },
    {
      driver: 'citus',
      schema: 'aSchema',
      tableName: 'aTableName',
      expected: { name: 'aTableName', schema: 'aSchema' },
    },
  ];

  it.each(tests)(
    'returns the qualified table',
    ({ driver, schema, tableName, expected }) => {
      expect(
        getQualifiedTable({
          driver,
          schema,
          tableName,
        })
      ).toEqual(expected);
    }
  );

  describe('when driver is bigquery', () => {
    it('returns the qualified table', () => {
      expect(
        getQualifiedTable({
          driver: 'bigquery',
          schema: 'aDataset',
          tableName: 'tableName',
        })
      ).toEqual({ name: 'tableName', dataset: 'aDataset' });
    });
  });
});
