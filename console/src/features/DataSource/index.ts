import { AxiosInstance } from 'axios';
import { z } from 'zod';
import { OpenApiSchema } from '@hasura/dc-api-types';
import { DataNode } from 'antd/lib/tree';
import { Source, SupportedDrivers, Table } from '@/features/MetadataAPI';
import { postgres, PostgresTable } from './postgres';
import { bigquery } from './bigquery';
import { citus } from './citus';
import { mssql } from './mssql';
import { gdc } from './gdc';
import { cockroach } from './cockroach';
import * as utils from './common/utils';
import type {
  // Property,
  IntrospectedTable,
  TableColumn,
  GetTrackableTablesProps,
  GetTableColumnsProps,
  TableFkRelationships,
  GetFKRelationshipProps,
  DriverInfoResponse,
  GetTablesListAsTreeProps,
  TableRow,
  GetTableRowsProps,
  WhereClause,
  OrderBy,
  Operator,
  GetSupportedOperatorsProps,
} from './types';

import {
  exportMetadata,
  NetworkArgs,
  RunSQLResponse,
  getDriverPrefix,
} from './api';
import { transformSchemaToZodObject } from '../OpenApi3Form/utils';
import { getAllSourceKinds } from './common/getAllSourceKinds';

export enum Feature {
  NotImplemented = 'Not Implemented',
}

export const nativeDrivers = [
  'postgres',
  'bigquery',
  'mssql',
  'citus',
  'cockroach',
];

export const supportedDrivers = [...nativeDrivers, 'gdc'];

export const getDriver = (dataSource: Source) => {
  if (nativeDrivers.includes(dataSource.kind)) {
    return dataSource.kind;
  }

  return 'gdc';
};

export type Database = {
  introspection?: {
    getDriverInfo: () => Promise<DriverInfoResponse | Feature.NotImplemented>;
    getDatabaseConfiguration: (
      httpClient: AxiosInstance,
      driver?: string
    ) => Promise<
      | {
          configSchema: OpenApiSchema;
          otherSchemas: Record<string, OpenApiSchema>;
        }
      | Feature.NotImplemented
    >;
    getTrackableTables: (
      props: GetTrackableTablesProps
    ) => Promise<IntrospectedTable[] | Feature.NotImplemented>;
    getDatabaseHierarchy: () => Promise<string[] | Feature.NotImplemented>;
    getTableColumns: (
      props: GetTableColumnsProps
    ) => Promise<TableColumn[] | Feature.NotImplemented>;
    getFKRelationships: (
      props: GetFKRelationshipProps
    ) => Promise<TableFkRelationships[] | Feature.NotImplemented>;
    getTablesListAsTree: (
      props: GetTablesListAsTreeProps
    ) => Promise<DataNode | Feature.NotImplemented>;
    getSupportedOperators: (
      props: GetSupportedOperatorsProps
    ) => Promise<Operator[] | Feature.NotImplemented>;
  };
  query?: {
    getTableRows: (
      props: GetTableRowsProps
    ) => Promise<TableRow[] | Feature.NotImplemented>;
  };
  modify?: null;
};

const drivers: Record<SupportedDrivers, Database> = {
  postgres,
  bigquery,
  citus,
  mssql,
  gdc,
  cockroach,
};

const getDatabaseMethods = async ({
  dataSourceName,
  httpClient,
}: { dataSourceName: string } & NetworkArgs) => {
  const { metadata } = await exportMetadata({ httpClient });

  const dataSource = metadata.sources.find(
    source => source.name === dataSourceName
  );

  if (!dataSource) {
    throw Error(
      'DataSource.introspectTables data source not found in metadata'
    );
  }

  if (nativeDrivers.includes(dataSource.kind)) return drivers[dataSource.kind];

  return drivers.gdc;
};

const getDriverMethods = (driver: SupportedDrivers) => {
  if (driver === 'pg') return drivers.postgres;

  if (nativeDrivers.includes(driver)) return drivers[driver];

  return drivers.gdc;
};

export const DataSource = (httpClient: AxiosInstance) => ({
  driver: {
    getAllSourceKinds: async () => {
      const serverSupportedDrivers = await getAllSourceKinds({ httpClient });
      const allDrivers = serverSupportedDrivers.map(async driver => {
        const driverInfo = await getDriverMethods(
          driver.kind
        ).introspection?.getDriverInfo();
        console.log(driver, driverInfo);

        if (driverInfo && driverInfo !== Feature.NotImplemented)
          return {
            name: driverInfo.name,
            displayName: driverInfo.displayName,
            release: driverInfo.release,
            native: driver.builtin,
          };

        return {
          name: driver.kind,
          displayName: driver.kind,
          release: 'Beta',
          native: driver.builtin,
        };
      });
      return Promise.all(allDrivers);
    },
  },
  getNativeDrivers: async () => {
    return nativeDrivers;
  },
  connectDB: {
    getConfigSchema: async (driver: string) => {
      const driverName = (
        nativeDrivers.includes(driver) ? driver : 'gdc'
      ) as SupportedDrivers;
      return drivers[driverName].introspection?.getDatabaseConfiguration(
        httpClient,
        driver
      );
    },
    getFormSchema: async (driver: string) => {
      const driverName = (
        nativeDrivers.includes(driver) ? driver : 'gdc'
      ) as SupportedDrivers;

      const schema = await drivers[
        driverName
      ].introspection?.getDatabaseConfiguration(httpClient, driver);

      if (schema === Feature.NotImplemented || !schema) return;

      return z.object({
        driver: z.literal(driver),
        name: z.string().min(1, 'Name is a required field!'),
        replace_configuration: z.preprocess(x => {
          if (!x) return false;
          return true;
        }, z.boolean()),
        configuration: transformSchemaToZodObject(
          schema.configSchema,
          schema.otherSchemas
        ),
        customization: z
          .object({
            root_fields: z
              .object({
                namespace: z.string().optional(),
                prefix: z.string().optional(),
                suffix: z.string().optional(),
              })
              .optional(),
            type_names: z
              .object({
                prefix: z.string().optional(),
                suffix: z.string().optional(),
              })
              .optional(),
          })
          .deepPartial()
          .optional(),
      });
    },
  },
  introspectTables: async ({ dataSourceName }: { dataSourceName: string }) => {
    const { metadata } = await exportMetadata({ httpClient });

    const dataSource = metadata.sources.find(
      source => source.name === dataSourceName
    );

    if (!dataSource) {
      throw Error(
        'DataSource.introspectTables data source not found in metadata'
      );
    }

    const kind = getDriver(dataSource);
    /* 
      NOTE: We need a set of metadata types. Until then dataSource is type-casted to `any` because `configuration` varies from DB to DB and the old metadata types contain 
      only pg databases at the moment. Changing the old types will require us to modify multiple legacy files
    */

    const getTrackableTables = drivers[kind].introspection?.getTrackableTables;

    if (getTrackableTables) {
      return getTrackableTables({
        dataSourceName: dataSource.name,
        configuration: dataSource.configuration,
        httpClient,
      });
    }

    return Feature.NotImplemented;
  },
  getDatabaseHierarchy: async ({
    dataSourceName,
  }: {
    dataSourceName: string;
  }) => {
    const { metadata } = await exportMetadata({ httpClient });

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
    configuration,
  }: {
    configuration?: any;
    dataSourceName: string;
    table: Table;
  }) => {
    const database = await getDatabaseMethods({ dataSourceName, httpClient });
    if (!database) return [];

    const introspection = database.introspection;
    if (!introspection) return [];

    const result = await introspection.getTableColumns({
      dataSourceName,
      configuration,
      table,
      httpClient,
    });
    if (result === Feature.NotImplemented) return [];
    return result;
  },
  getTableFkRelationships: async ({
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

    const result = await introspection.getFKRelationships({
      dataSourceName,
      table,
      httpClient,
    });

    if (result === Feature.NotImplemented) return [];

    return result;
  },
  getTablesWithHierarchy: async ({
    dataSourceName,
  }: {
    dataSourceName: string;
  }) => {
    const database = await getDatabaseMethods({ dataSourceName, httpClient });

    if (!database) return null;

    const introspection = database.introspection;

    if (!introspection) return null;

    const treeData = await introspection.getTablesListAsTree({
      dataSourceName,
      httpClient,
    });

    if (treeData === Feature.NotImplemented) return null;

    return treeData;
  },
  getTableRows: async ({
    dataSourceName,
    table,
    columns,
    options,
  }: {
    dataSourceName: string;
    table: Table;
    columns: string[];
    options?: {
      where?: WhereClause[];
      offset?: number;
      limit?: number;
      order_by?: OrderBy[];
    };
  }) => {
    const database = await getDatabaseMethods({ dataSourceName, httpClient });

    if (!database) throw Error('Database not found!');

    const query = database.query;

    if (!query) return Feature.NotImplemented;

    const tableRows = await query.getTableRows({
      dataSourceName,
      table,
      columns,
      httpClient,
      options,
    });

    return tableRows;
  },
  getSupportedOperators: async ({
    dataSourceName,
  }: {
    dataSourceName: string;
  }) => {
    const database = await getDatabaseMethods({ dataSourceName, httpClient });

    if (!database) throw Error('Database not found!');

    const introspection = database.introspection;

    if (!introspection) return Feature.NotImplemented;

    const operators = await introspection.getSupportedOperators({ httpClient });

    return operators;
  },
});

export {
  exportMetadata,
  utils,
  RunSQLResponse,
  getDriverPrefix,
  PostgresTable,
};

export * from './types';
export * from './guards';
