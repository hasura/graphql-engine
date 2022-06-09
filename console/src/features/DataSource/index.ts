import { Primitive, ZodDiscriminatedUnionOption } from 'zod';
import { AxiosInstance } from 'axios';
import { postgres } from './postgres';
import { bigquery } from './bigquery';
import { citus } from './citus';
import { mssql } from './mssql';
import { gdc } from './gdc';

// This will be modified in the upcoming changes to connectDB GDC feature
type UISchema = {
  key: string;
  label: string;
};

export enum Feature {
  NotImplemented = 'Not Implemented',
}

export type SupportedDrivers =
  | 'postgres'
  | 'bigquery'
  | 'mssql'
  | 'citus'
  | 'gdc';

export const supportedDrivers = [
  'postgres',
  'bigquery',
  'mssql',
  'citus',
  'gdc',
];

export type Database = {
  /* This section defines how a database is connected to Hasura */
  connectDB: {
    /* Expose Methods that are used in the DB connection part */
    getUISchema: (
      fetch: AxiosInstance
    ) => Promise<UISchema[] | Feature.NotImplemented>;
    getValidationSchema: (
      fetch: AxiosInstance
    ) => Promise<
      ZodDiscriminatedUnionOption<'driver', Primitive> | Feature.NotImplemented
    >;
  };
};

const drivers: Record<SupportedDrivers, Database> = {
  postgres,
  bigquery,
  citus,
  mssql,
  gdc,
};

export const DataSource = (hasuraFetch: AxiosInstance) => ({
  driver: {
    getSupportedDrivers: async () => {
      return supportedDrivers;
    },
  },
  connectDB: {
    getUISchema: async (driver: SupportedDrivers) => {
      return drivers[driver].connectDB.getUISchema(hasuraFetch);
    },
    getValidationSchema: async (driver: SupportedDrivers) => {
      return drivers[driver].connectDB.getValidationSchema(hasuraFetch);
    },
  },
});
