import { GraphQLType } from 'graphql';
import {
  Legacy_SourceToRemoteSchemaRelationship,
  LocalTableArrayRelationship,
  LocalTableObjectRelationship,
  ManualArrayRelationship,
  ManualObjectRelationship,
  SameTableObjectRelationship,
  Source,
  SourceToRemoteSchemaRelationship,
  SourceToSourceRelationship,
  Table,
  TableFunction,
  SupportedDriver,
  QualifiedTable,
  Where,
  OrderBy,
} from '@hasura/shared/types';
import { ColumnValueGenerationStrategy } from '@hasura/dc-api-types';
import { Endpoints } from '@hasura/shared/context';
import { FetchJson } from '@hasura/shared/utils';

export type DataSourceNetworkArgs<T = any> = {
  endpoints: Endpoints;
  fetchJson: FetchJson<T>;
};

export type AllowedTableRelationships =
  /**
   * Object relationships between columns of the same table. There is no same-table arr relationships
   */
  | SameTableObjectRelationship
  /**
   * Object relationships between columns of two different tables but the tables are in the same DB
   */
  | LocalTableObjectRelationship
  /**
   * Array relationships between columns of two different tables but the tables are in the same DB using FKs
   */
  | LocalTableArrayRelationship
  /**
   * Manually added Object relationships between columns of two different tables but the tables are in the same DB FKs
   */
  | ManualObjectRelationship
  /**
   * Manually added Array relationships between columns of two different tables but the tables are in the same DB
   */
  | ManualArrayRelationship
  /**
   * Manually added relationships between columns of two different tables and the tables are in different DBs
   */
  | SourceToSourceRelationship
  /**
   * Manually added relationships between a DB and a remote schema - there are two formats as per the server.
   */
  | Legacy_SourceToRemoteSchemaRelationship
  | SourceToRemoteSchemaRelationship;

export type IntrospectedTable = {
  name: string;
  table: Table;
  type: string;
};

export type TableColumn = {
  /**
   * Name of the column as defined in the DB
   */
  name: string;
  /**
   * dataType of the column as defined in the DB
   */
  dataType: string | { type: string; name: string };
  /**
   * console data type: the dataType property is group into one of these types and console uses this internally
   */
  consoleDataType:
    | 'string'
    | 'text'
    | 'json'
    | 'integer'
    | 'number'
    | 'boolean'
    | 'float'
    | 'timestamp'
    | 'geography'
    | 'uuid'
    | 'date'
    | 'time'
    | 'array';
  nullable?: boolean;
  /**
   * Full SQL type including modifiers (e.g. `character varying(255)`), when
   * the driver can introspect it. Falls back to `dataType` otherwise.
   */
  sqlType?: string;
  /**
   * Raw SQL default expression (e.g. `now()`), `null` when the column has no
   * default. `undefined` means the driver does not introspect defaults.
   */
  defaultValue?: string | null;
  isPrimaryKey?: boolean;
  graphQLProperties?: {
    name: string;
    scalarType?: string;
    graphQLType?: GraphQLType | undefined;
  };
  value_generated?: ColumnValueGenerationStrategy;
};

export type TableColumnTypeMap = Record<
  TableColumn['consoleDataType'],
  string[]
>;

export type GetTableCommentProps = {
  dataSourceName: string;
  table: Table;
} & DataSourceNetworkArgs;

export type GetViewCommentProps = GetTableCommentProps;

export type GetViewDefinitionProps = {
  dataSourceName: string;
  table: Table;
} & DataSourceNetworkArgs;

export type GetFunctionDefinitionProps = {
  dataSourceName: string;
  func: TableFunction;
} & DataSourceNetworkArgs;

export type GetFunctionCommentProps = GetFunctionDefinitionProps;

export type GetFunctionDefinitionResult = {
  definition: string;
  isVolatile?: boolean;
  returnTable?: Table;
};

export type GetViewDefinitionResult = {
  definition: string;
};

export type GetTrackableTablesProps = {
  dataSourceName: string;
  configuration?: any;
} & DataSourceNetworkArgs;

export type GetTableColumnsProps = {
  dataSourceName: string;
  table: Table;
} & DataSourceNetworkArgs;

export type GetTablesListAsTreeProps = {
  dataSourceName: string;
  releaseName?: ReleaseType;
} & DataSourceNetworkArgs;

export type ReleaseType = 'GA' | 'Beta' | 'Alpha' | 'disabled';

export type DriverInfo = {
  name: SupportedDriver;
  displayName: string;
  release: ReleaseType;
  native?: boolean;
  available?: boolean;
  enterprise?: boolean;
};

export type GetTableRowsProps = {
  table: Table;
  dataSourceName: string;
  columns: TableColumn[];
  options?: {
    where?: Where;
    offset?: number;
    limit?: number;
    order_by?: OrderBy[];
  };
} & DataSourceNetworkArgs;
export type TableRow = Record<string, string | number | boolean>;

export type validOperators = string;
export type SelectColumn = string | { name: string; columns: SelectColumn[] };

export type Operator = {
  name: string;
  value: string;
  defaultValue?: string;
};

export type Version = string;
export type GetVersionProps = {
  dataSourceName: string;
} & DataSourceNetworkArgs;

export type GetDefaultQueryRootProps = {
  table: Table;
};

export type GetTrackableFunctionProps = {
  dataSourceName: string;
} & DataSourceNetworkArgs;

export type IntrospectedFunction = {
  name: string;
  function: TableFunction;
  isVolatile: boolean;
};

export type GetDatabaseSchemaProps = {
  dataSourceName: string;
} & DataSourceNetworkArgs;

export type ChangeDatabaseSchemaProps = {
  dataSourceName: string;
  schemaName: string;
  isMigration: boolean;
  cascade?: boolean;
} & DataSourceNetworkArgs;

export type DropFunctionProps = {
  dataSourceName: string;
  func: TableFunction;
  isMigration: boolean;
} & DataSourceNetworkArgs;

export type DropTableProps = {
  dataSourceName: string;
  table: Table;
  isMigration: boolean;
  cascade?: boolean;
} & DataSourceNetworkArgs;

export type ChangeTableNameProps = {
  dataSourceName: string;
  table: Omit<IntrospectedTable, 'name'>;
  isMigration: boolean;
  newName: string;
} & DataSourceNetworkArgs;

export type GetIsTableViewProps = {
  dataSourceName: string;
  table: Table;
} & DataSourceNetworkArgs;

export type GetSupportedDataTypesProps = {
  driver: SupportedDriver;
} & DataSourceNetworkArgs;

export type GetSupportedScalarsProps = GetSupportedDataTypesProps;

export type GetStoredProceduresProps = {
  dataSourceName: string;
} & DataSourceNetworkArgs;

export type GetDatabaseConfigurationProps = DataSourceNetworkArgs & {
  driver: string;
};
export type GetDriverCapabilitiesArgs = GetDatabaseConfigurationProps;

export type GetTrackableObjectsProps = DataSourceNetworkArgs & {
  dataSourceName: string;
};

export type GetTrackableObjectsResponse = {
  tables: {
    name: string;
    table: Table;
    type: string;
  }[];
  functions: IntrospectedFunction[];
};

export type InsertRowArgs = {
  source: Omit<Source, 'tables'>;
  table: Table;
  objects: Record<string, unknown>[];
  columns: TableColumn[];
  defaultColumns: string[];
};

export type InsertRowProps = DataSourceNetworkArgs & {
  args: InsertRowArgs;
};

export type UpdateRowArgs = {
  source: Omit<Source, 'tables'>;
  table: Table;
  set: Record<string, unknown>;
  where: Record<string, unknown>;
  columns: TableColumn[];
  defaultColumns: string[];
};

export type UpdateRowProps = DataSourceNetworkArgs & {
  args: UpdateRowArgs;
};

export type DeleteRowArgs = {
  source: Omit<Source, 'tables'>;
  table: Table;
  columns: TableColumn[];
  where: Record<string, unknown>;
};

export type DeleteRowProps = DataSourceNetworkArgs & {
  args: DeleteRowArgs;
};

/**
 * Legacy supported feature types for native drivers.
 */
export type SupportedFeaturesType = {
  tables: {
    view: boolean;
    create: {
      enabled: boolean;
      arrayTypes: boolean;
    };
    browse: {
      enabled: boolean;
      customPagination?: boolean;
      aggregation: boolean;
      deleteRow: boolean;
      editRow: boolean;
      bulkRowSelect: boolean;
    };
    insert: {
      enabled: boolean;
    };
    modify: {
      enabled: boolean;
      readOnly?: boolean;
      columns?: {
        view: boolean;
        edit: boolean;
        graphqlFieldName: boolean;
      };
      computedFields?: boolean;
      primaryKeys?: {
        view: boolean;
        edit: boolean;
      };
      foreignKeys?: {
        view: boolean;
        edit: boolean;
      };
      uniqueKeys?: {
        view: boolean;
        edit: boolean;
      };
      triggers?: boolean;
      checkConstraints?: {
        view: boolean;
        edit: boolean;
      };
      indexes?: {
        view: boolean;
        edit: boolean;
      };
      customGqlRoot?: boolean;
      setAsEnum?: boolean;
      untrack?: boolean;
      delete?: boolean;
    };
    relationships: {
      enabled: boolean;
      remoteDbRelationships?: {
        hostSource: boolean;
        referenceSource: boolean;
      };
      remoteRelationships?: boolean;
      track: boolean;
    };
    permissions: {
      enabled: boolean;
      aggregation: boolean;
    };
    track: {
      enabled: boolean;
    };
  };
  functions: {
    track: {
      enabled: boolean;
    };
  };
  events: {
    triggers: {
      add: boolean;
    };
  };
  actions: {
    relationships: boolean;
  };
  rawSQL: {
    tracking: boolean;
  };
  connectDbForm: ConnectDbForm;
};

type ConnectDbForm = {
  enabled: boolean;
  connectionParameters: boolean;
  databaseURL: boolean;
  environmentVariable: boolean;
  read_replicas: {
    create: boolean;
    edit: boolean;
  };
  extensions_schema: boolean;
  namingConvention: boolean;
} & DbConnectionSettings;

export type DbConnectionSettings = {
  connectionSettings: boolean;
  cumulativeMaxConnections: boolean;
  retries: boolean;
  pool_timeout: boolean;
  connection_lifetime: boolean;
  isolation_level: boolean;
  prepared_statements: boolean;
  ssl_certificates: boolean;
};

export type TableORSchemaArg =
  { schemas: string[] } | { tables: QualifiedTable[] };

export type ParseCreateSchemaSQLResult = {
  type: 'table' | 'view' | 'function';
  schema: string;
  name: string;
  isPartition: boolean;
};

export type ValidateInputRowValuesFunction = (data: {
  values: Record<string, unknown>;
  columns: TableColumn[];
  graphqlMode: boolean;
}) => Record<string, unknown>;
