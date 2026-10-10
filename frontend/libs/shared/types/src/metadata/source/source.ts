import type {
  BigQueryConfiguration,
  CitusConfiguration,
  MssqlConfiguration,
  PostgresConfiguration,
} from './configuration';
import type { LogicalModel } from './logicalModel';
import type { NativeQuery } from './nativeQuery';
import type { StoredProcedure } from './storedProcedure';
import type { MetadataTable, Table } from './table';

export const POSTGRESQL_FAMILY_DRIVERS = [
  'postgres',
  'citus',
  'alloy',
  'cockroach',
] as const;

export const NATIVE_DRIVERS = [
  ...POSTGRESQL_FAMILY_DRIVERS,
  'mssql',
  'bigquery',
] as const;

// Kind of source represents the name of both native source and data source connectors.
// Some of kind uses the same driver, such as Postgres and Alloy uses the driver.
export type PostgresFamilyDriver = (typeof POSTGRESQL_FAMILY_DRIVERS)[number];
export type NativeDriver = (typeof NATIVE_DRIVERS)[number];

export const KNOWN_ENTERPRISE_DRIVERS = [
  'snowflake',
  'athena',
  'mysql',
  'mariadb',
  'redshift',
  'mongodb',
  'oracle',
  // SQLite is a test data connector driver. It is not available in enterprise.
  'sqlite',
] as const;

export const SUPPORTED_DRIVERS = [
  ...NATIVE_DRIVERS,
  ...KNOWN_ENTERPRISE_DRIVERS,
] as const;

export type KnownEnterpriseDriver = (typeof KNOWN_ENTERPRISE_DRIVERS)[number];
export type SupportedDriver = (typeof SUPPORTED_DRIVERS)[number];

export type NamingConvention = 'hasura-default' | 'graphql-default';

export type SourceCustomization = {
  root_fields?: {
    namespace?: string;
    prefix?: string;
    suffix?: string;
  };
  type_names?: {
    prefix?: string;
    suffix?: string;
  };
  naming_convention?: NamingConvention;
};

export type TableFunction = Table;

export type FunctionConfiguration = {
  custom_name?: string;
  comment?: string;
  custom_root_fields?: {
    function?: string;
    function_aggregate?: string;
  };
  session_argument?: string;
  exposed_as?: 'mutation' | 'query';
  response?: {
    type: 'table';
    table: Table;
  };
};

export type MetadataFunction = {
  function: TableFunction;
  configuration?: FunctionConfiguration;
  permissions?: FunctionPermission[];
};

export type FunctionPermission = {
  role: string;
  definition?: Record<string, any>;
};

type BaseSource = {
  name: string;
  tables: MetadataTable[];
  customization?: SourceCustomization;
  functions?: MetadataFunction[];
  logical_models?: LogicalModel[];
  native_queries?: NativeQuery[];
  stored_procedures?: StoredProcedure[];
};

export type PostgresSource = BaseSource & {
  kind: 'postgres';
  configuration: PostgresConfiguration;
};

export type AlloySource = BaseSource & {
  kind: 'alloy';
  configuration: unknown;
};

export type MssqlSource = BaseSource & {
  kind: 'mssql';
  configuration: MssqlConfiguration;
};

export type BigQuerySource = BaseSource & {
  kind: 'bigquery';
  configuration: BigQueryConfiguration;
};

export type CitusSource = BaseSource & {
  kind: 'citus';
  configuration: CitusConfiguration;
};

export type CockroachSource = BaseSource & {
  kind: 'cockroach';
  configuration: unknown;
};

export type GDCSource = BaseSource & {
  /**
   * This will still return string. This is implemented for readability reasons.
   * Until TS has negated types, any string will be considered as gdc
   */
  kind: Exclude<KnownEnterpriseDriver, NativeDriver>;
  configuration: unknown;
  logical_models?: never;
  native_queries?: never;
};

export type Source =
  | PostgresSource
  | AlloySource
  | MssqlSource
  | BigQuerySource
  | CitusSource
  | CockroachSource
  | GDCSource;

export type { LogicalModel, LogicalModelField } from './logicalModel';
export type { NativeQuery, NativeQueryArgument } from './nativeQuery';
export type {
  QualifiedStoredProcedure,
  StoredProcedure,
  StoredProcedureArgument,
} from './storedProcedure';
export type { LocalArrayRelationship, LocalObjectRelationship } from './table';

export type MetadataError = {
  code: string;
  error: string;
  path: string;
};
export type BulkKeepGoingResponse = (
  | {
      message: 'success';
    }
  | MetadataError
)[];

export type BulkAtomicResponse =
  | {
      message: 'success';
    }
  | MetadataError;

export const isBulkAtomicResponseError = (
  response: BulkAtomicResponse,
): response is MetadataError => {
  return 'error' in response;
};

export type { NativeQueryRelationship } from './nativeQuery';

export type QualifiedDataSource = {
  name: string;
  kind: SupportedDriver;
};
