import { Capabilities, OpenApiSchema } from '@hasura/dc-api-types';
import type { TreeDataNode } from '@hasura/shared/ui';
import {
  DataQueryType,
  RunSQLResponse,
  StoredProcedure,
  Table,
  Path,
  DeepRequired,
} from '@hasura/shared/types';
import {
  ChangeDatabaseSchemaProps,
  DriverInfo,
  GetDatabaseSchemaProps,
  GetDefaultQueryRootProps,
  GetTableColumnsProps,
  GetTableRowsProps,
  GetTablesListAsTreeProps,
  GetTrackableFunctionProps,
  GetTrackableTablesProps,
  IntrospectedFunction,
  GetVersionProps,
  // Property,
  IntrospectedTable,
  Operator,
  TableColumn,
  TableRow,
  Version,
  GetStoredProceduresProps,
  GetSupportedScalarsProps,
  GetDatabaseConfigurationProps,
  GetDriverCapabilitiesArgs,
  SupportedFeaturesType,
  ParseCreateSchemaSQLResult,
  TableColumnTypeMap,
  GetTrackableObjectsProps,
  GetTrackableObjectsResponse,
  UpdateRowProps,
  InsertRowProps,
  GetFunctionDefinitionProps,
  DropFunctionProps,
  GetFunctionDefinitionResult,
  DropTableProps,
  GetTableCommentProps,
  GetViewCommentProps,
  GetFunctionCommentProps,
  ChangeTableNameProps,
  DeleteRowProps,
  GetViewDefinitionProps,
  GetViewDefinitionResult,
} from './types';
import {
  ModifyForeignKeyProps,
  ViolationAction,
  CreateForeignKeyProps,
  GetFKRelationshipProps,
  TableFkRelationships,
} from './relationship';
import { TableType } from '../../types';
import {
  FrequentlyUsedColumn,
  GetCheckConstraintsProps,
  TableCheckConstraint,
  GetTableKeysProps,
  TableKeyConstraint,
  GetTableIndexesProps,
  TableIndex,
  GetTableTriggersProps,
  GetTriggerFunctionsProps,
  TriggerFunction,
  TableTrigger,
} from './introspection';
import {
  CreateCheckConstraintProps,
  DropCheckConstraintProps,
  CreatePrimaryKeyProps,
  AlterPrimaryKeyProps,
  AlterColumnProps,
  AddColumnProps,
  DropColumnProps,
  DropPrimaryKeyProps,
  CreateUniqueKeyProps,
  DropUniqueKeyProps,
  CreateIndexProps,
  DropIndexProps,
  DropTriggerProps,
  CreateTriggerProps,
  ModifyTableProps,
} from './table';

export type Database = {
  introspection: {
    getVersion?: (props: GetVersionProps) => Promise<Version>;
    getDriverInfo?: () => Promise<DriverInfo>;
    getDatabaseConfiguration?: (
      props: GetDatabaseConfigurationProps,
    ) => Promise<{
      configSchema: OpenApiSchema;
      otherSchemas: Record<string, OpenApiSchema>;
    }>;
    getDriverCapabilities: (
      props: GetDriverCapabilitiesArgs,
    ) => Promise<Capabilities>;
    getTrackableTables?: (
      props: GetTrackableTablesProps,
    ) => Promise<IntrospectedTable[]>;
    getDatabaseHierarchy?: () => string[];
    getTableColumns: (props: GetTableColumnsProps) => Promise<TableColumn[]>;
    getTableColumnInfos: (
      props: GetTableColumnsProps,
    ) => Promise<TableColumn[]>;
    getFKRelationships?: (
      props: GetFKRelationshipProps,
    ) => Promise<TableFkRelationships[]>;
    getCheckConstraints?: (
      props: GetCheckConstraintsProps,
    ) => Promise<TableCheckConstraint[]>;
    getPrimaryKey?: (
      props: GetTableKeysProps,
    ) => Promise<TableKeyConstraint | null>;
    getUniqueKeys?: (props: GetTableKeysProps) => Promise<TableKeyConstraint[]>;
    getTableIndexes?: (props: GetTableIndexesProps) => Promise<TableIndex[]>;
    getTableTriggers?: (
      props: GetTableTriggersProps,
    ) => Promise<TableTrigger[]>;
    getTriggerFunctions?: (
      props: GetTriggerFunctionsProps,
    ) => Promise<TriggerFunction[]>;
    getTablesListAsTree: (
      props: GetTablesListAsTreeProps,
    ) => Promise<TreeDataNode>;
    getSupportedOperators: () => Operator[];
    getTrackableFunctions?: (
      props: GetTrackableFunctionProps,
    ) => Promise<IntrospectedFunction[]>;
    getTrackableObjects?: (
      props: GetTrackableObjectsProps,
    ) => Promise<GetTrackableObjectsResponse>;
    getDatabaseSchemas?: (props: GetDatabaseSchemaProps) => Promise<string[]>;
    getSupportedDataTypes: (
      props: GetSupportedScalarsProps,
    ) => Promise<TableColumnTypeMap>;
    getSupportedScalars: (props: GetSupportedScalarsProps) => Promise<string[]>;
    getStoredProcedures?: (
      props: GetStoredProceduresProps,
    ) => Promise<StoredProcedure[]>;
    getFunctionDefinition?: (
      props: GetFunctionDefinitionProps,
    ) => Promise<GetFunctionDefinitionResult>;
    /** The database (SQL) comment of a table. Comments shown in the console
     *  come from metadata `configuration.comment`; this is only used to
     *  import the database comment on demand. */
    getTableComment?: (
      props: GetTableCommentProps,
    ) => Promise<string | undefined>;
    /** The database (SQL) comment of a view or materialized view. */
    getViewComment?: (
      props: GetViewCommentProps,
    ) => Promise<string | undefined>;
    /** The database (SQL) comment of a function. */
    getFunctionComment?: (
      props: GetFunctionCommentProps,
    ) => Promise<string | undefined>;
    getViewDefinition?: (
      props: GetViewDefinitionProps,
    ) => Promise<GetViewDefinitionResult>;
  };
  query: {
    getTableRows: (props: GetTableRowsProps) => Promise<TableRow[]>;
  };
  modify?: {
    createDatabaseSchema?: (
      props: ChangeDatabaseSchemaProps,
    ) => Promise<RunSQLResponse>;
    deleteDatabaseSchema?: (
      props: ChangeDatabaseSchemaProps,
    ) => Promise<RunSQLResponse>;
    createTable?: (props: ModifyTableProps) => Promise<boolean>;
    modifyTable?: (props: ModifyTableProps) => Promise<boolean>;
    changeTableName?: (props: ChangeTableNameProps) => Promise<boolean>;
    addColumn?: (props: AddColumnProps) => Promise<boolean>;
    alterColumn?: (props: AlterColumnProps) => Promise<boolean>;
    dropColumn?: (props: DropColumnProps) => Promise<boolean>;
    dropTable?: (props: DropTableProps) => Promise<boolean>;
    dropFunction?: (props: DropFunctionProps) => Promise<boolean>;
    insertRows?: (props: InsertRowProps) => Promise<number>;
    updateRows?: (props: UpdateRowProps) => Promise<number>;
    deleteRows?: (props: DeleteRowProps) => Promise<number>;
    createForeignKey?: (props: CreateForeignKeyProps) => Promise<boolean>;
    alterForeignKey?: (props: ModifyForeignKeyProps) => Promise<boolean>;
    dropForeignKey?: (props: ModifyForeignKeyProps) => Promise<boolean>;
    createCheckConstraint?: (
      props: CreateCheckConstraintProps,
    ) => Promise<boolean>;
    dropCheckConstraint?: (props: DropCheckConstraintProps) => Promise<boolean>;
    createPrimaryKey?: (props: CreatePrimaryKeyProps) => Promise<boolean>;
    alterPrimaryKey?: (props: AlterPrimaryKeyProps) => Promise<boolean>;
    dropPrimaryKey?: (props: DropPrimaryKeyProps) => Promise<boolean>;
    createUniqueKey?: (props: CreateUniqueKeyProps) => Promise<boolean>;
    dropUniqueKey?: (props: DropUniqueKeyProps) => Promise<boolean>;
    createIndex?: (props: CreateIndexProps) => Promise<boolean>;
    dropIndex?: (props: DropIndexProps) => Promise<boolean>;
    createTrigger?: (props: CreateTriggerProps) => Promise<boolean>;
    dropTrigger?: (props: DropTriggerProps) => Promise<boolean>;
  };
  config: {
    getDefaultQueryRoot: (props: GetDefaultQueryRootProps) => string;
    getSupportedQueryTypes: (table: Table) => DataQueryType[];
    getViolationActions: () => ViolationAction[];
    getFrequentlyUsedColumns?: () => FrequentlyUsedColumn[];
  };
  check: {
    isFeatureSupported: (
      feature: Path<DeepRequired<SupportedFeaturesType>>,
    ) => boolean;
    isSchemaModification: (sql: string) => boolean;
    isTable: (tableType: string) => boolean;
  };
  utilities: {
    parseCreateSchemaSQL?: (sql: string) => ParseCreateSchemaSQLResult[];
    statementTimeoutSQL?(statementTimeoutInSecs: number): string;
  };
};

export interface DatasourceSqlQueries {
  statementTimeout?: (statementTimeoutInSecs: number) => string;
  renameTableOrView?: (
    tableType: TableType,
    schemaName: string,
    oldName: string,
    newName: string,
  ) => string;
}

// Re-exported so existing `@hasura/metadata/data-source` imports keep working.
// It lives in `@hasura/shared/types` so that `@hasura/metadata/api` can use it
// without depending on this package (which itself depends on the api package).
export { NotImplementedError } from '@hasura/shared/types';
