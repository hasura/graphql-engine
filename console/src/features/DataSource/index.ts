import { AxiosInstance } from 'axios';
import { z, ZodSchema } from 'zod';
import { postgres } from './postgres';
import { bigquery } from './bigquery';
import { citus } from './citus';
import { mssql } from './mssql';
import { gdc } from './gdc';
import * as utils from './common/utils';
import type {
  Property,
  Ref,
  OneOf,
  IntrospectedTable,
  MetadataTable,
  Table,
  SupportedDrivers,
  TableColumn,
  GetTrackableTablesProps,
  GetTableColumnsProps,
} from './types';

import { createZodSchema } from './common/createZodSchema';
import { exportMetadata, NetworkArgs } from './api';

export enum Feature {
  NotImplemented = 'Not Implemented',
}

const nativeDrivers = ['postgres', 'bigquery', 'mssql', 'citus'];

export const supportedDrivers = [...nativeDrivers, 'gdc'];

export type Database = {
  introspection?: {
    getDatabaseConfiguration: (
      fetch: AxiosInstance
    ) => Promise<
      | { configSchema: Property; otherSchemas: Record<string, Property> }
      | Feature.NotImplemented
    >;
    getTrackableTables: (
      props: GetTrackableTablesProps
    ) => Promise<IntrospectedTable[] | Feature.NotImplemented>;
    getDatabaseHierarchy: () => Promise<string[] | Feature.NotImplemented>;
    getTableColumns: (
      props: GetTableColumnsProps
    ) => Promise<TableColumn[] | Feature.NotImplemented>;
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

const getDatabaseMethods = async ({
  dataSourceName,
  httpClient,
}: { dataSourceName: string } & NetworkArgs) => {
  const metadata = await exportMetadata({ httpClient });

  const dataSource = metadata.sources.find(
    source => source.name === dataSourceName
  );

  if (!dataSource) {
    throw Error(
      'DataSource.introspectTables data source not found in metadata'
    );
  }

  return drivers[dataSource.kind];
};

export const DataSource = (httpClient: AxiosInstance) => ({
  driver: {
    getSupportedDrivers: async () => {
      return supportedDrivers;
    },
  },
  getNativeDrivers: async () => {
    return nativeDrivers;
  },
  connectDB: {
    getConfigSchema: async (driver: SupportedDrivers) => {
      return drivers[driver].introspection?.getDatabaseConfiguration(
        httpClient
      );
    },
    getFormSchema: async () => {
      const schemas = await Promise.all(
        Object.values(drivers).map(database =>
          database.introspection?.getDatabaseConfiguration(httpClient)
        )
      );

      let res: ZodSchema | undefined;

      schemas.forEach(schema => {
        if (schema === Feature.NotImplemented || !schema) return;

        const postgresSchema = z.object({
          driver: z.literal('postgres'),
          name: z.string().min(1, 'Name is a required field!'),
          replace_configuration: z.preprocess(x => {
            if (!x) return false;
            return true;
          }, z.boolean()),
          configuration: createZodSchema(
            schema.configSchema,
            schema.otherSchemas
          ),
        });

        if (!res) {
          res = postgresSchema;
        } else {
          res = res.or(postgresSchema);
        }
      });

      return res;
    },
  },
  introspectTables: async ({ dataSourceName }: { dataSourceName: string }) => {
    const metadata = await exportMetadata({ httpClient });

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
      return getTrackableTables({
        dataSourceName: dataSource.name,
        configuration: (dataSource as any).configuration,
        httpClient,
      });
    return Feature.NotImplemented;
  },
  getDatabaseHierarchy: async ({
    dataSourceName,
  }: {
    dataSourceName: string;
  }) => {
    const metadata = await exportMetadata({ httpClient });

    const dataSource = metadata.sources.find(
      source => source.name === dataSourceName
    );

    if (!dataSource) {
      throw Error(
        'DataSource.introspectTables data source not found in metadata'
      );
    }

    const database = drivers[dataSource.kind];
    if (!database) return [];

    const introspection = database.introspection;
    if (!introspection) return [];

    const result = await introspection.getDatabaseHierarchy();
    if (result === Feature.NotImplemented) return [];

    return result;
  },
  getTableColumns: async ({
    dataSourceName,
    table,
  }: {
    dataSourceName: string;
    table: Table;
  }) => {
    const database = await getDatabaseMethods({ dataSourceName, httpClient });
    if (!database) return [];

    const introspection = database.introspection;
    if (!introspection) return [];

    const result = await introspection.getTableColumns({
      dataSourceName,
      table,
      httpClient,
    });
    if (result === Feature.NotImplemented) return [];

    return result;
  },
});

export { exportMetadata, utils };
export type {
  Property,
  Ref,
  OneOf,
  SupportedDrivers,
  IntrospectedTable,
  Table,
  MetadataTable,
  NetworkArgs,
};
