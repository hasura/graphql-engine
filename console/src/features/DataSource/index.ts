import { AxiosInstance } from 'axios';
import { z, ZodSchema } from 'zod';
import { postgres } from './postgres';
import { bigquery } from './bigquery';
import { citus } from './citus';
import { mssql } from './mssql';
import { gdc } from './gdc';
import type {
  Property,
  Ref,
  OneOf,
  IntrospectedTable,
  Table,
  SupportedDrivers,
} from './types';
import { getZodSchema } from './common/utils';
import { exportMetadata } from './api';

export enum Feature {
  NotImplemented = 'Not Implemented',
}

export const supportedDrivers = [
  'postgres',
  'bigquery',
  'mssql',
  'citus',
  'gdc',
];

export type Database = {
  introspection?: {
    getDatabaseConfiguration: (
      fetch: AxiosInstance
    ) => Promise<
      | { configSchema: Property; otherSchemas: Record<string, Property> }
      | Feature.NotImplemented
    >;
    getTrackableTables: (
      dataSourceName: string,
      configuration: any
    ) => Promise<IntrospectedTable[] | Feature.NotImplemented>;
  };
  query?: {
    getTableData: () => void;
  };
  modify?: null;
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
      return drivers[driver].introspection?.getDatabaseConfiguration(
        hasuraFetch
      );
    },
    getFormSchema: async () => {
      const schemas = await Promise.all(
        Object.values(drivers).map(database =>
          database.introspection?.getDatabaseConfiguration(hasuraFetch)
        )
      );

      let res: ZodSchema | undefined;

      schemas.forEach(schema => {
        if (schema === Feature.NotImplemented || !schema) return;

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
  introspectTables: async ({ dataSourceName }: { dataSourceName: string }) => {
    const metadata = await exportMetadata();

    const dataSource = metadata.sources.find(
      source => source.name === dataSourceName
    );

    if (!dataSource) {
      throw Error(
        'DataSource.introspectTables data source not found in metadata'
      );
    }

    /* 
      NOTE: We need a set of metadata types. Until then dataSource is type-casted to `any` because `configuration` varies from DB to DB and the old metadata types contain 
      only pg databases at the moment. Changing the old types will require us to modify multiple legacy files
    */
    const getTrackableTables =
      drivers[dataSource.kind].introspection?.getTrackableTables;
    if (getTrackableTables)
      return getTrackableTables(
        dataSource.name,
        (dataSource as any).configuration
      );
    return Feature.NotImplemented;
  },
});

export { exportMetadata };
export type {
  Property,
  Ref,
  OneOf,
  SupportedDrivers,
  IntrospectedTable,
  Table,
};
