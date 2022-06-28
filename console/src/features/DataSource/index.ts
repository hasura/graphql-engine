import { AxiosInstance } from 'axios';
import { z, ZodSchema } from 'zod';
import { postgres } from './postgres';
import { bigquery } from './bigquery';
import { citus } from './citus';
import { mssql } from './mssql';
import { gdc } from './gdc';
import { Property, Ref, OneOf } from './types';
import { getZodSchema } from './utils';

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
    getConfigSchema: (
      fetch: AxiosInstance
    ) => Promise<
      | { configSchema: Property; otherSchemas: Record<string, Property> }
      | Feature.NotImplemented
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
    getConfigSchema: async (driver: SupportedDrivers) => {
      return drivers[driver].connectDB.getConfigSchema(hasuraFetch);
    },
    getFormSchema: async () => {
      const schemas = await Promise.all(
        Object.values(drivers).map(database =>
          database.connectDB.getConfigSchema(hasuraFetch)
        )
      );

      let res: ZodSchema | undefined;

      schemas.forEach(schema => {
        if (schema === Feature.NotImplemented) return;

        if (!res) {
          res = z.object({
            driver: z.literal('postgres'),
            name: z.string().min(1, 'Name is a required field!'),
            replace_configuration: z.preprocess(x => {
              if (!x) return false;
              return true;
            }, z.boolean()),
            configuration: getZodSchema(
              schema.configSchema,
              schema.otherSchemas
            ),
          });
        } else {
          res = res.or(
            z.object({
              driver: z.literal('postgres'),
              name: z.string().min(1, 'Name is a required field!'),
              replace_configuration: z.preprocess(x => {
                if (!x) return false;
                return true;
              }, z.boolean()),
              configuration: getZodSchema(
                schema.configSchema,
                schema.otherSchemas
              ),
            })
          );
        }
      });

      return res;
    },
  },
});

export { Property, Ref, OneOf };
