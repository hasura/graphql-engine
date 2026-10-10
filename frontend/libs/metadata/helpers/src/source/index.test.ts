import { Source } from '@hasura/shared/types';
import {
  getDriverPrefix,
  isBigQuerySource,
  isCitusSource,
  isCockroachSource,
  isKnownEnterpriseSourceKind,
  isMssqlSource,
  isNativeDriver,
  isPostgresFlavour,
  isPostgresSource,
} from './index';

const sourceOfKind = (kind: string): Source =>
  ({ name: kind, kind, tables: [], configuration: {} }) as unknown as Source;

describe('isPostgresFlavour', () => {
  it.each`
    driver         | expected
    ${'postgres'}  | ${true}
    ${'citus'}     | ${true}
    ${'cockroach'} | ${true}
    ${'alloy'}     | ${true}
    ${'mssql'}     | ${false}
    ${'bigquery'}  | ${false}
    ${'snowflake'} | ${false}
    ${''}          | ${false}
  `('returns $expected for $driver', ({ driver, expected }) => {
    expect(isPostgresFlavour(driver)).toBe(expected);
  });
});

describe('getDriverPrefix', () => {
  it('returns "pg" for any postgres-flavoured driver', () => {
    expect(getDriverPrefix('postgres')).toBe('pg');
    expect(getDriverPrefix('citus')).toBe('pg');
    expect(getDriverPrefix('cockroach')).toBe('pg');
    expect(getDriverPrefix('alloy')).toBe('pg');
  });

  it('returns the driver name itself for non-postgres drivers', () => {
    expect(getDriverPrefix('mssql')).toBe('mssql');
    expect(getDriverPrefix('bigquery')).toBe('bigquery');
  });
});

describe('source kind narrowers', () => {
  it('isPostgresSource', () => {
    expect(isPostgresSource(sourceOfKind('postgres'))).toBe(true);
    expect(isPostgresSource(sourceOfKind('citus'))).toBe(false);
  });

  it('isMssqlSource', () => {
    expect(isMssqlSource(sourceOfKind('mssql'))).toBe(true);
    expect(isMssqlSource(sourceOfKind('postgres'))).toBe(false);
  });

  it('isBigQuerySource', () => {
    expect(isBigQuerySource(sourceOfKind('bigquery'))).toBe(true);
    expect(isBigQuerySource(sourceOfKind('postgres'))).toBe(false);
  });

  it('isCitusSource', () => {
    expect(isCitusSource(sourceOfKind('citus'))).toBe(true);
    expect(isCitusSource(sourceOfKind('postgres'))).toBe(false);
  });

  it('isCockroachSource', () => {
    expect(isCockroachSource(sourceOfKind('cockroach'))).toBe(true);
    expect(isCockroachSource(sourceOfKind('postgres'))).toBe(false);
  });
});

describe('isNativeDriver', () => {
  it.each`
    driver         | expected
    ${'postgres'}  | ${true}
    ${'citus'}     | ${true}
    ${'alloy'}     | ${true}
    ${'cockroach'} | ${true}
    ${'mssql'}     | ${true}
    ${'bigquery'}  | ${true}
    ${'snowflake'} | ${false}
    ${'mongodb'}   | ${false}
  `('returns $expected for $driver', ({ driver, expected }) => {
    expect(isNativeDriver(driver)).toBe(expected);
  });
});

describe('isKnownEnterpriseSourceKind', () => {
  it.each`
    driver         | expected
    ${'snowflake'} | ${true}
    ${'athena'}    | ${true}
    ${'mysql'}     | ${true}
    ${'mariadb'}   | ${true}
    ${'redshift'}  | ${true}
    ${'mongodb'}   | ${true}
    ${'oracle'}    | ${true}
    ${'sqlite'}    | ${true}
    ${'postgres'}  | ${false}
    ${'mssql'}     | ${false}
  `('returns $expected for $driver', ({ driver, expected }) => {
    expect(isKnownEnterpriseSourceKind(driver)).toBe(expected);
  });
});
