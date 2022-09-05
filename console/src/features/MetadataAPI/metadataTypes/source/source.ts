import {
  BigQueryConfiguration,
  CitusConfiguration,
  MssqlConfiguration,
  PostgresConfiguration,
} from './configuration';
import { MetadataTable } from './table';

export type NativeDrivers =
  | 'postgres'
  | 'mssql'
  | 'bigquery'
  | 'citus'
  | 'cockroach';
export type GDCDriver = string;
export type SupportedDrivers = NativeDrivers | GDCDriver;

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
  naming_convention?: string;
};

export type PGFunction = {
  function: string | { name: string; schema: string };
  configuration?: {
    custom_name?: string;
    custom_root_fields?: {
      function?: string;
      function_aggregate?: string;
    };
    session_argument?: string;
    exposed_as?: 'mutation' | 'query';
  };
};

export type Source = {
  name: string;
  tables: MetadataTable[];
  customization?: SourceCustomization;
} & (
  | {
      kind: 'postgres';
      configuration: PostgresConfiguration;
      functions?: PGFunction[];
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
    }
);
