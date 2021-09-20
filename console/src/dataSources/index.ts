/* eslint-disable import/no-mutable-exports */
import { useState, useEffect } from 'react';
import { DeepRequired } from 'ts-essentials';

import { Path, get } from '../components/Common/utils/tsUtils';
import { services } from './services';

import {
  Table,
  ComputedField,
  TableColumn,
  FrequentlyUsedColumn,
  IndexType,
  PermissionColumnCategories,
  SupportedFeaturesType,
  generateTableRowRequestType,
  BaseTableColumn,
  generateInsertRequestType,
  GenerateRowsCountRequestType,
  GenerateEditRowRequest,
  GenerateDeleteRowRequest,
  GenerateBulkDeleteRowRequest,
  ViolationActions,
  IndexFormTips as IndexFormToolTips,
} from './types';
import { PGFunction, FunctionState } from './services/postgresql/types';
import { Operations } from './common';
import { QualifiedTable } from '../metadata/types';

import { supportedFeatures as PGSupportedFeatures } from './services/postgresql';
import { supportedFeatures as MssqlSupportedFeatures } from './services/mssql';
import { supportedFeatures as BigQuerySupportedFeatures } from './services/bigquery';
import { supportedFeatures as CitusQuerySupportedFeatures } from './services/citus';

export const drivers = [
  'postgres',
  'mysql',
  'mssql',
  'bigquery',
  'citus',
] as const;
export type Driver = typeof drivers[number];

export const driverToLabel: Record<Driver, string> = {
  mysql: 'MySQL',
  postgres: 'PostgreSQL',
  mssql: 'MS SQL Server',
  bigquery: 'BigQuery',
  citus: 'Citus',
};

export const sourceNames = {
  postgres: PGSupportedFeatures?.driver?.name,
  mssql: MssqlSupportedFeatures?.driver?.name,
  bigquery: BigQuerySupportedFeatures?.driver?.name,
  citus: CitusQuerySupportedFeatures?.driver?.name,
};

export type ColumnsInfoResult = {
  [tableName: string]: {
    [columnName: string]: {
      is_generated: boolean;
      is_identity: boolean;
      identity_generation: 'ALWAYS' | 'BY DEFAULT' | string | null;
    };
  };
};

export interface DataSourcesAPI {
  isTable(table: Table): boolean;
  displayTableName(table: Table): JSX.Element;
  // todo: replace with function type
  getFunctionSchema(func: PGFunction): string;
  getFunctionDefinition(func: PGFunction): string;
  getSchemaFunctions(
    func: PGFunction[],
    schemaName: string,
    tableName: string,
    tableSchema: string
  ): PGFunction[];
  findFunction(
    func: PGFunction[],
    fName: string,
    fSchema: string
  ): PGFunction | undefined;
  getGroupedTableComputedFields(
    table: Table,
    allFunctions: PGFunction[]
  ): {
    scalar: ComputedField[];
    table: ComputedField[];
  };
  isColumnAutoIncrement(col: TableColumn): boolean;
  getTableSupportedQueries(table: Table): Operations[];
  getColumnType(col: TableColumn): string;
  arrayToPostgresArray(arr: any[]): string;
  schemaListSql(schemas: string[]): string;
  getAdditionalColumnsInfoQuerySql?: (currentSchema: string) => string;
  parseColumnsInfoResult: (data: string[][]) => ColumnsInfoResult;
  columnDataTypes: {
    INTEGER?: string;
    SERIAL?: string;
    BIGINT?: string;
    JSONDTYPE?: string;
    TIMESTAMP?: string;
    NUMERIC?: string;
    DATE?: string;
    BOOLEAN?: string;
    TEXT?: string;
    ARRAY?: string;
    BIGSERIAL?: string;
    DATETIME?: string;
    JSONB?: string;
    UUID?: string;
    TIME?: string;
    TIMETZ?: string;
  };
  operators: Array<{ name: string; value: string; graphqlOp: string }>;
  getFetchTablesListQuery: (options: {
    schemas: string[];
    tables: Table[];
  }) => string;
  commonDataTypes: {
    name: string;
    value: string;
    description: string;
  }[];
  fetchColumnTypesQuery: string;
  fetchColumnDefaultFunctions(schema: string): string;
  isSQLFunction(str: string): boolean;
  isJsonColumn(column: BaseTableColumn): boolean;
  getEstimateCountQuery: (schemaName: string, tableName: string) => string;
  isColTypeString(colType: string): boolean;
  cascadeSqlQuery(sql: string): string;
  dependencyErrorCode: string;
  getCreateTableQueries: (
    currentSchema: string,
    tableName: string,
    columns: any[],
    primaryKeys: (number | string)[],
    foreignKeys: any[],
    uniqueKeys: any[],
    checkConstraints: any[],
    tableComment?: string | undefined
  ) =>
    | string[]
    | {
        error: string;
      };
  getDropTableSql(schema: string, table: string): string;
  createSQLRegex: RegExp;
  getStatementTimeoutSql?: (statementTimeoutInSecs: number) => string;
  getDropSchemaSql(schema: string): string;
  getCreateSchemaSql(schema: string): string;
  isTimeoutError: (error: any) => boolean;
  getAlterForeignKeySql: (
    from: {
      tableName: string;
      schemaName: string;
      columns: string[];
    },
    to: {
      tableName: string;
      schemaName: string;
      columns: string[];
    },
    dropConstraint: string,
    newConstraint: string,
    onUpdate: string,
    onDelete: string
  ) => string;
  getCreateFKeySql: (
    from: {
      tableName: string;
      schemaName: string;
      columns: string[];
    },
    to: {
      tableName: string;
      schemaName: string;
      columns: string[];
    },
    newConstraint: string,
    onUpdate: string,
    onDelete: string
  ) => string;
  getDropConstraintSql: (
    tableName: string,
    schemaName: string,
    constraintName: string
  ) => string;
  getRenameTableSql: (
    property: string | undefined,
    schemaName: string,
    oldName: string,
    newName: string
  ) => string;
  getDropTriggerSql: (
    tableSchema: string,
    triggerName: string,
    tableName?: string
  ) => string;
  getCreateTriggerSql: (
    tableName: string,
    tableSchema: string,
    triggerName: string,
    trigger: {
      action_timing: string;
      event_manipulation: string;
      action_orientation: string;
      action_statement: string;
      comment?: string;
    }
  ) => string;
  getDropSql: (
    tableName: string,
    schemaName: string,
    property?: string | undefined
  ) => string;
  getViewDefinitionSql: (viewName: string) => string;
  getDropColumnSql: (
    tableName: string,
    schemaName: string,
    columnName: string
  ) => string;
  getAddColumnSql: (
    tableName: string,
    schemaName: string,
    columnName: string,
    columnType: string,
    options?: {
      nullable: boolean;
      unique: boolean;
      default: any;
      sqlGenerator?: FrequentlyUsedColumn['dependentSQLGenerator'];
    },
    constraintName?: string
  ) => string | string[];
  getDropNotNullSql: (
    tableName: string,
    schemaName: string,
    columnName: string,
    columnType?: string
  ) => string;
  getSetNotNullSql: (
    tableName: string,
    schemaName: string,
    columnName: string,
    columnType: string
  ) => string;
  getAddUniqueConstraintSql: (
    tableName: string,
    schemaName: string,
    constraintName: string,
    columns: string[]
  ) => string;
  getSetColumnDefaultSql: (
    tableName: string,
    schemaName: string,
    columnName: string,
    defaultValue: any,
    constraintName: string
  ) => string;
  getSetCommentSql: (
    on: 'table' | 'column' | string,
    tableName: string,
    schemaName: string,
    columnName: string,
    comment: string,
    columnType?: string
  ) => string;
  getAlterColumnTypeSql: (
    tableName: string,
    schemaName: string,
    columnName: string,
    columnType: string,
    wasNullable?: boolean
  ) => string;
  getDropColumnDefaultSql: (
    tableName: string,
    schemaName: string,
    columnName: string,
    constraintName?: string,
    defaultValue?: string
  ) => string;
  getRenameColumnQuery: (
    tableName: string,
    schemaName: string,
    newName: string,
    oldName: string
  ) => string;
  fetchColumnCastsQuery: string;
  checkSchemaModification: (sql: string) => boolean;
  getCreateCheckConstraintSql: (
    tableName: string,
    schemaName: string,
    constraintName: string,
    check: string
  ) => string;
  getCreatePkSql: ({
    schemaName,
    tableName,
    selectedPkColumns,
    constraintName,
  }: {
    schemaName: string;
    tableName: string;
    selectedPkColumns: string[];
    constraintName?: string;
  }) => string;
  getAlterPkSql: ({
    schemaName,
    tableName,
    selectedPkColumns,
    constraintName,
  }: {
    schemaName: string;
    tableName: string;
    selectedPkColumns: string[];
    constraintName: string;
  }) => string;
  getFunctionDefinitionSql:
    | null
    | ((
        schemaName: string,
        functionName?: string | null | undefined,
        type?: 'trackable' | 'non-trackable' | undefined
      ) => string);
  frequentlyUsedColumns: FrequentlyUsedColumn[];
  primaryKeysInfoSql: (options: {
    schemas: string[];
    tables: Table[];
  }) => string;
  uniqueKeysSql: (options: { schemas: string[]; tables: Table[] }) => string;
  checkConstraintsSql?: (options: {
    schemas: string[];
    tables: Table[];
  }) => string;
  tableIndexSql?: (options: { schema: string; table: string }) => string;
  createIndexSql?: (indexInfo: {
    indexName: string;
    indexType: IndexType;
    table: QualifiedTable;
    columns: string[];
    unique?: boolean;
  }) => string;
  dropIndexSql?: (indexName: string) => string;
  indexFormToolTips?: IndexFormToolTips;
  indexTypes?: Record<string, IndexType>;
  supportedIndex?: {
    multiColumn: string[];
    singleColumn: string[];
  };
  getFKRelations: (options: { schemas: string[]; tables: Table[] }) => string;
  getReferenceOption: (opt: string) => string;
  deleteFunctionSql?: (
    schemaName: string,
    functionState: FunctionState
  ) => string;
  getEventInvocationInfoByIDSql?: (
    logTableDef: QualifiedTable,
    eventLogTable: QualifiedTable,
    eventId: string
  ) => string;
  getDatabaseInfo: string;
  getTableInfo?: (tables: QualifiedTable[]) => string;
  generateTableRowRequest?: () => generateTableRowRequestType;
  getDatabaseVersionSql?: string;
  permissionColumnDataTypes: Partial<PermissionColumnCategories> | null;
  viewsSupported: boolean;
  // use null, if all operators are supported
  supportedColumnOperators: string[] | null;
  supportedFeatures?: DeepRequired<SupportedFeaturesType>;
  violationActions: ViolationActions[];
  defaultRedirectSchema?: string;
  generateInsertRequest?: () => generateInsertRequestType;
  generateRowsCountRequest?: () => GenerateRowsCountRequestType;
  getPartitionDetailsSql?: (tableName: string, tableSchema: string) => string;
  generateEditRowRequest?: () => GenerateEditRowRequest;
  generateDeleteRowRequest?: () => GenerateDeleteRowRequest;
  generateBulkDeleteRowRequest?: () => GenerateBulkDeleteRowRequest;
}

export let currentDriver: Driver = 'postgres';
export let dataSource: DataSourcesAPI = services[currentDriver || 'postgres'];

export const isFeatureSupported = (
  feature: Path<DeepRequired<SupportedFeaturesType>>
) => {
  if (dataSource.supportedFeatures)
    return get(dataSource.supportedFeatures, feature);
};

export const getSupportedDrivers = (feature: Path<SupportedFeaturesType>) =>
  [
    PGSupportedFeatures,
    MssqlSupportedFeatures,
    BigQuerySupportedFeatures,
    CitusQuerySupportedFeatures,
  ].reduce((driverList: Driver[], supportedFeaturesObj) => {
    if (get(supportedFeaturesObj, feature)) {
      return [...driverList, supportedFeaturesObj.driver.name];
    }
    return driverList;
  }, []);

class DataSourceChangedEvent extends Event {
  static type = 'data-source-changed';
  constructor(public driver: Driver) {
    super(DataSourceChangedEvent.type);
  }
}
const eventTarget = new EventTarget();

export const setDriver = (driver: Driver) => {
  currentDriver = driver;
  dataSource = services[driver];

  eventTarget.dispatchEvent(new DataSourceChangedEvent(driver));
};

export const useDataSource = (): {
  driver: Driver;
  setDriver: (driver: Driver) => void;
  dataSource: DataSourcesAPI;
} => {
  const [driver, setState] = useState(currentDriver);

  useEffect(() => {
    const handleDriverChange = (event: Event) => {
      if (event instanceof DataSourceChangedEvent) {
        setState(event.driver);
      }
    };

    eventTarget.addEventListener(
      DataSourceChangedEvent.type,
      handleDriverChange
    );

    return () => {
      eventTarget.removeEventListener(
        DataSourceChangedEvent.type,
        handleDriverChange
      );
    };
  });

  return {
    driver,
    dataSource,
    setDriver,
  };
};

if ((module as any).hot) {
  // // todo
  // (module as any).hot.dispose((data: any) => {
  //   data.driver = currentDriver;
  // });
  // (module as any).hot.accept(['./postgres'], () => {
  //   currentDriver = (module as any).hot.data.driver;
  // });
}

export * from './common';
