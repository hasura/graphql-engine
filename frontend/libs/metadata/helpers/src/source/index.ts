import {
  BigQuerySource,
  CitusSource,
  CockroachSource,
  KNOWN_ENTERPRISE_DRIVERS,
  KnownEnterpriseDriver,
  MssqlSource,
  NATIVE_DRIVERS,
  NativeDriver,
  PostgresFamilyDriver,
  PostgresSource,
  Source,
  SupportedDriver,
} from '@hasura/shared/types';

export const isPostgresFlavour = (
  driver: string,
): driver is PostgresFamilyDriver =>
  driver === 'postgres' ||
  driver === 'citus' ||
  driver === 'cockroach' ||
  driver === 'alloy';

export const getDriverPrefix = (driver: string) =>
  isPostgresFlavour(driver)
    ? 'pg'
    : (driver as Exclude<SupportedDriver, PostgresFamilyDriver>);

export function isPostgresSource(source: Source): source is PostgresSource {
  return source.kind === 'postgres';
}

export function isMssqlSource(source: Source): source is MssqlSource {
  return source.kind === 'mssql';
}

export function isBigQuerySource(source: Source): source is BigQuerySource {
  return source.kind === 'bigquery';
}

export function isCitusSource(source: Source): source is CitusSource {
  return source.kind === 'citus';
}

export function isCockroachSource(source: Source): source is CockroachSource {
  return source.kind === 'cockroach';
}

export function isNativeDriver(input: string): input is NativeDriver {
  return NATIVE_DRIVERS.includes(input as NativeDriver);
}

export function isKnownEnterpriseSourceKind(
  input: string,
): input is KnownEnterpriseDriver {
  return KNOWN_ENTERPRISE_DRIVERS.includes(input as KnownEnterpriseDriver);
}
