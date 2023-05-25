import { Driver } from '../../../dataSources';
import {
  BigQueryConfiguration,
  CitusConfiguration,
  MssqlConfiguration,
  PostgresConfiguration,
} from './configuration';
import { LogicalModel } from './logicalModel';
import { NativeQuery } from './nativeQuery';
import { MetadataTable, Table } from './table';
import { StoredProcedure } from './storedProcedure';

export type NativeDrivers =
  | 'postgres'
  | 'alloy'
  | 'mssql'
  | 'mysql'
  | 'bigquery'
  | 'citus'
  | 'cockroach';

export type SuperConnectorDrivers = 'snowflake' | 'athena' | 'mysql8' | string;

export type SupportedDrivers = Driver | SuperConnectorDrivers;

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

export type MetadataFunction = {
  function: QualifiedFunction;
  configuration?: {
    custom_name?: string;
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
};

export type Source = {
  name: string;
  tables: MetadataTable[];
  customization?: SourceCustomization;
  functions?: MetadataFunction[];
  logical_models?: LogicalModel[];
  native_queries?: NativeQuery[];
  stored_procedures?: StoredProcedure[];
} & (
  | {
      kind: 'postgres';
      configuration: PostgresConfiguration;
    }
  | {
      kind: 'mssql';
      configuration: MssqlConfiguration;
    }
  | {
      kind: 'bigquery';
      configuration: BigQueryConfiguration;
    }
  | {
      kind: 'citus';
      configuration: CitusConfiguration;
    }
  | {
      /**
       * This will still return string. This is implemented for readability reasons.
       * Until TS has negated types, any string will be considered as gdc
       */
      kind: Exclude<SupportedDrivers, NativeDrivers>;
      configuration: unknown;
      logical_models?: never;
      native_queries?: never;
    }
);

export type QualifiedFunction = unknown;
export type { LogicalModel, LogicalModelField } from './logicalModel';
export type { NativeQuery, NativeQueryArgument } from './nativeQuery';
export type {
  StoredProcedure,
  StoredProcedureArgument,
  QualifiedStoredProcedure,
} from './storedProcedure';

export type BulkKeepGoingResponse = [
  | {
      message: 'success';
    }
  | {
      code: string;
      error: string;
      path: string;
    }
];
