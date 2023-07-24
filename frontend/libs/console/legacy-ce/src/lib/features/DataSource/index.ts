import { Capabilities, OpenApiSchema } from '@hasura/dc-api-types';
import { DataNode } from 'antd/lib/tree';
import { AxiosInstance } from 'axios';
import pickBy from 'lodash/pickBy';
import { z } from 'zod';
import { Source, SupportedDrivers, Table } from '../hasura-metadata-types';
import { AlloyDbTable, alloy } from './alloydb';
import { bigquery } from './bigquery';
import { citus } from './citus';
import { cockroach } from './cockroach';
import { gdc } from './gdc';
import { mssql } from './mssql';
import { postgres } from './postgres';
import type {
  ChangeDatabaseSchemaProps,
  DriverInfo,
  GetDatabaseSchemaProps,
  GetDefaultQueryRootProps,
  GetFKRelationshipProps,
  GetSupportedOperatorsProps,
  GetTableColumnsProps,
  GetTableRowsProps,
  GetTablesListAsTreeProps,
  GetTrackableFunctionProps,
  GetTrackableTablesProps,
  IntrospectedFunction,
  GetVersionProps,
  GetIsTableViewProps,
  // Property,
  IntrospectedTable,
  Operator,
  OrderBy,
  TableColumn,
  TableFkRelationships,
  TableRow,
  Version,
  WhereClause,
  StoredProcedure,
  GetStoredProceduresProps,
  GetSupportedScalarsProps,
} from './types';

import { transformSchemaToZodObject } from '../OpenApi3Form/utils';
import { QueryType } from '../Permissions/types';
import {
  NetworkArgs,
  RunSQLAPIError,
  RunSQLCommandResponse,
  RunSQLResponse,
  RunSQLSelectResponse,
  exportMetadata,
  getDriverPrefix,
  runGraphQL,
  runIntrospectionQuery,
  runMetadataQuery,
} from './api';
import { getAllSourceKinds } from './common/getAllSourceKinds';
import { getTableName } from './common/getTableName';
import { ReleaseType } from './types';
import {
  GetTrackableObjectsProps,
  GetTrackableObjectsResponse,
} from './gdc/introspection/getTrackableObjects';
import { isObject } from '../../components/Common/utils/jsUtils';

export * from './common/utils';
export type { GDCTable } from './gdc';
export * from './guards';
export type { PostgresTable } from './postgres';
export * from './types';
export {
  exportMetadata,
  runGraphQL,
  getTableName,
  runMetadataQuery,
  getDriverPrefix,
  runIntrospectionQuery,
};
export type {
  RunSQLResponse,
  RunSQLSelectResponse,
  RunSQLCommandResponse,
  NetworkArgs,
  AlloyDbTable,
  RunSQLAPIError,
};
export enum Feature {
  NotImplemented = 'Not Implemented',
}

export const nativeDrivers = [
  'postgres',
  'bigquery',
  'mssql',
  'citus',
  'cockroach',
  'alloy',
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
    getVersion?: (
      props: GetVersionProps
    ) => Promise<Version | Feature.NotImplemented>;
    getDriverInfo: () => Promise<DriverInfo | Feature.NotImplemented>;
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
    getDriverCapabilities: (
      httpClient: AxiosInstance,
      driver?: string
    ) => Promise<Capabilities | Feature>;
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
    getTrackableFunctions: (
      props: GetTrackableFunctionProps
    ) => Promise<IntrospectedFunction[] | Feature.NotImplemented>;
    getTrackableObjects: (
      props: GetTrackableObjectsProps
    ) => Promise<GetTrackableObjectsResponse | Feature.NotImplemented>;
    getDatabaseSchemas: (
      props: GetDatabaseSchemaProps
    ) => Promise<string[] | Feature.NotImplemented>;
    getIsTableView: (
      props: GetIsTableViewProps
    ) => Promise<boolean | Feature.NotImplemented>;
    getSupportedDataTypes: () => Promise<
      Record<TableColumn['consoleDataType'], string[]> | Feature.NotImplemented
    >;
    getSupportedScalars: (
      props: GetSupportedScalarsProps
    ) => Promise<string[] | Feature.NotImplemented>;
    getStoredProcedures: (
      props: GetStoredProceduresProps
    ) => Promise<StoredProcedure[] | Feature.NotImplemented>;
  };
  query?: {
    getTableRows: (
      props: GetTableRowsProps
    ) => Promise<TableRow[] | Feature.NotImplemented>;
  };
  modify?: {
    defaultQueryRoot: (props: GetDefaultQueryRootProps) => Promise<string>;
    createDatabaseSchema: (
      props: ChangeDatabaseSchemaProps
    ) => Promise<RunSQLResponse | Feature.NotImplemented>;
    deleteDatabaseSchema: (
      props: ChangeDatabaseSchemaProps
    ) => Promise<RunSQLResponse | Feature.NotImplemented>;
  };
  config: {
    getDefaultQueryRoot: (
      table: Table
    ) => Promise<string | Feature.NotImplemented>;
    getSupportedQueryTypes: (
      table: Table
    ) => Promise<QueryType[] | Feature.NotImplemented>;
  };
};

const drivers: Record<SupportedDrivers, Database> = {
  postgres,
  bigquery,
  citus,
  mssql,
  gdc,
  cockroach,
  alloy,
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
    getAllSourceKinds: async (): Promise<DriverInfo[]> => {
      const serverSupportedDrivers = await getAllSourceKinds({ httpClient });
      const knownEnterpriseDrivers = [
        'athena',
        'snowflake',
        'mysql8',
        'mariadb',
        'oracle',
      ];
      const allSupportedDrivers = serverSupportedDrivers
        // NOTE: AlloyDB is added here and not returned by the server because it's not a new data source (it's Postgres)
        .concat([
          {
            builtin: true,
            kind: 'alloy',
            display_name: 'AlloyDB',
            available: true,
          },
        ])
        .sort((a, b) => (a.kind > b.kind ? 1 : -1));

      const allDrivers = allSupportedDrivers.map(async driver => {
        const driverInfo = await getDriverMethods(
          driver.kind
        ).introspection?.getDriverInfo();

        if (driverInfo && driverInfo !== Feature.NotImplemented)
          return {
            name: driverInfo.name,
            displayName: driverInfo.displayName,
            release: driverInfo.release,
            native: driver.builtin,
            available: true,
            enterprise: false,
          };

        return {
          name: driver.kind,
          displayName: driver.display_name,
          release: (driver.release_name as ReleaseType) ?? 'GA',
          native: driver.builtin,
          available: driver.available,
          enterprise: knownEnterpriseDrivers.includes(driver.kind),
        };
      });
      return Promise.all(allDrivers);
    },
  },
  getNativeDrivers: async () => {
    return nativeDrivers;
  },
  getDatabaseVersion: async (
    dataSourceName: string
  ): Promise<string | Feature.NotImplemented> => {
    const database = await getDatabaseMethods({ dataSourceName, httpClient });
    return (
      database.introspection?.getVersion?.({ dataSourceName, httpClient }) ??
      Feature.NotImplemented
    );
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
              .transform(value => pickBy(value, d => d !== ''))
              .optional(),
            type_names: z
              .object({
                prefix: z.string().optional(),
                suffix: z.string().optional(),
              })
              .transform(value => pickBy(value, d => d !== ''))
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
      throw Error(`${dataSourceName} not found in metadata`);
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
    releaseName,
  }: {
    dataSourceName: string;
    releaseName?: ReleaseType;
  }) => {
    const database = await getDatabaseMethods({ dataSourceName, httpClient });

    if (!database) return null;

    const introspection = database.introspection;

    if (!introspection) return null;

    const treeData = await introspection.getTablesListAsTree({
      dataSourceName,
      httpClient,
      releaseName,
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
  getDefaultQueryRoot: async ({
    dataSourceName,
    table,
  }: {
    dataSourceName: string;
    table: Table;
  }) => {
    const database = await getDatabaseMethods({ dataSourceName, httpClient });

    return database.config.getDefaultQueryRoot(table);
  },
  getSupportedQueryTypes: async ({
    dataSourceName,
    table,
  }: {
    dataSourceName: string;
    table: Table;
  }) => {
    const database = await getDatabaseMethods({ dataSourceName, httpClient });

    return database.config.getSupportedQueryTypes(table);
  },
  getDriverCapabilities: async (driver: string) => {
    const driverName = (
      nativeDrivers.includes(driver) ? driver : 'gdc'
    ) as SupportedDrivers;

    return drivers[driverName].introspection?.getDriverCapabilities(
      httpClient,
      driver
    );
  },
  getTrackableFunctions: async (dataSourceName: string) => {
    const functions: IntrospectedFunction[] = [];
    const database = await getDatabaseMethods({ dataSourceName, httpClient });

    const trackableFunctions =
      (await database.introspection?.getTrackableFunctions({
        dataSourceName,
        httpClient,
      })) ?? [];

    if (Array.isArray(trackableFunctions)) {
      functions.push(...trackableFunctions);
    }

    const getTrackableObjectsFn = database.introspection?.getTrackableObjects;

    if (getTrackableObjectsFn) {
      const trackableObjects = await getTrackableObjectsFn({
        dataSourceName,
        httpClient,
      });

      if (isObject(trackableObjects) && 'functions' in trackableObjects) {
        functions.push(...trackableObjects.functions);
      }
    }

    return functions;
  },
  getDatabaseSchemas: async ({
    dataSourceName,
  }: {
    dataSourceName: string;
  }) => {
    const database = await getDatabaseMethods({ dataSourceName, httpClient });

    if (!database.introspection?.getDatabaseSchemas)
      throw new Error(`getDatabaseSchema() not callable for ${dataSourceName}`);

    return database.introspection?.getDatabaseSchemas({
      dataSourceName,
      httpClient,
    });
  },
  modifyDatabase: async ({ dataSourceName }: { dataSourceName: string }) => {
    const database = await getDatabaseMethods({ dataSourceName, httpClient });

    if (!database.modify)
      throw new Error(`modify methods are not callable for ${dataSourceName}`);

    return database.modify;
  },
  getIsTableView: async ({
    dataSourceName,
    table,
    httpClient,
  }: {
    dataSourceName: string;
    table: Table;
  } & NetworkArgs) => {
    const database = await getDatabaseMethods({ dataSourceName, httpClient });

    if (!database) return false;

    const introspection = database.introspection;

    if (!introspection) return false;

    const isView = await introspection.getIsTableView({
      dataSourceName,
      httpClient,
      table,
    });

    if (isView === Feature.NotImplemented) return false;

    return isView;
  },
  getSupportedDataTypes: async ({
    dataSourceName,
  }: {
    dataSourceName: string;
  }) => {
    const database = await getDatabaseMethods({ dataSourceName, httpClient });
    return (
      database.introspection?.getSupportedDataTypes() ?? Feature.NotImplemented
    );
  },
  getSupportedScalars: async ({
    dataSourceName,
  }: {
    dataSourceName: string;
  }) => {
    const database = await getDatabaseMethods({ dataSourceName, httpClient });
    const { metadata } = await exportMetadata({ httpClient });
    const dataSource = metadata.sources.find(
      source => source.name === dataSourceName
    );

    if (!dataSource) {
      throw Error(`Data source ${dataSource} not found in metadata`);
    }

    return (
      database.introspection?.getSupportedScalars({
        dataSourceKind: dataSource.kind,
        httpClient,
      }) ?? Feature.NotImplemented
    );
  },
  getStoredProcedures: async ({
    dataSourceName,
  }: {
    dataSourceName: string;
  }) => {
    const database = await getDatabaseMethods({ dataSourceName, httpClient });
    return (
      database.introspection?.getStoredProcedures({
        dataSourceName,
        httpClient,
      }) ?? Feature.NotImplemented
    );
  },
});
