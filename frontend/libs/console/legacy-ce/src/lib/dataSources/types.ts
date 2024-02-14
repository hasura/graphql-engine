import { Nullable } from '../components/Common/utils/tsUtils';
import { Column } from '../utils/postgresColumnTypes';
import {
  FunctionDefinition,
  QualifiedTable,
  RemoteRelationshipDef,
  TableConfig,
} from '../metadata/types';
import { ReduxState } from '../types';
import {
  getSelectQuery,
  getRunSqlQuery,
  WhereClause,
} from '../components/Common/utils/v1QueryUtils';
import { Driver } from '.';
import { PostgresTrigger } from './services/postgresql/types';
import { SourceCustomization } from '../features/hasura-metadata-types';

export type TableORSchemaArg =
  | { schemas: string[] }
  | { tables: QualifiedTable[] };

export interface Relationship
  extends Pick<BaseTable, 'table_name' | 'table_schema'> {
  rel_name: string;
  rel_def: {
    manual_configuration?: any;
    foreign_key_constraint_on?: any;
  };
  rel_type: 'object' | 'array';
}

export type Permission = {
  table_name: string;
  table_schema: string;
  role_name: string;
  permissions: {
    [action: string]: any;
  };
};

export interface BaseTableColumn {
  column_name: string;
  data_type: string;
  data_type_name?: string;
}

export type TableType =
  | 'TABLE'
  | 'VIEW'
  | 'MATERIALIZED VIEW'
  | 'FOREIGN TABLE'
  | 'PARTITIONED TABLE'
  | 'BASE TABLE'
  | 'TABLE' // specific to SQL Server
  | 'EXTERNAL'; // specific to Big Query

export interface ViewInfo {
  is_trigger_insertable_into: 'YES' | 'NO';
  is_insertable_into: 'YES' | 'NO';
  is_updatable: 'YES' | 'NO';
  is_trigger_updatable: 'YES' | 'NO';
  is_trigger_deletable: 'YES' | 'NO';
  table_schema: string;
  table_name: string;
  view_definition: string;
}

export interface NormalizedTable {
  table_schema: string;
  table_name: string;
  table_type: TableType;
  comment?: string | null;
  columns: TableColumn[];
  view_info?: ViewInfo | null;
  is_table_tracked: boolean;
  triggers?: PostgresTrigger[];
  citus_table_type?: string;
  configuration?: {
    column_config: ColumnConfig;
    custom_root_fields: CustomRootFields;
    custom_name: string;
  };
}

export interface TableColumn extends BaseTableColumn {
  is_generated?: boolean;
  is_nullable?: 'NO' | 'YES' | string;
  is_identity?: boolean;
  identity_generation?: 'ALWAYS' | 'BY DEFAULT' | null;
  comment?: string | null;
  data_type: string; // 'integer';
  data_type_name: string; // data_type = data_type_name for mysql
  table_name: string;
  column_name: string;
  table_schema: string;
  column_default?: string | null;
  ordinal_position: number;
  /**
   * auto_increment for columns that have the AUTO_INCREMENT attribute.
   * on update CURRENT_TIMESTAMP for TIMESTAMP or DATETIME columns that have the ON UPDATE CURRENT_TIMESTAMP attribute.
   * STORED GENERATED or VIRTUAL GENERATED for generated columns.
   * DEFAULT_GENERATED for columns that have an expression default value.
   */
  extra?: 'auto_increment' | string | null; // mysql
}

export type ForeignKeyConstraint = {
  column_mapping: {
    [lcol: string]: string;
  };
  table_schema: string;
  table_name: string;
  constraint_name: string;
  ref_table_table_schema: string;
  ref_table: string;
  on_update: string;
  on_delete: string;
  is_table_tracked: boolean;
  is_ref_table_tracked: boolean;
};

export type CheckConstraint = {
  table_schema: string;
  table_name: string;
  constraint_name: string;
  column_name?: string | null;
  /** sql definition */
  check: string;
};

export type ComputedField = {
  name?: string;
  computed_field_name: string;
  definition: {
    function: FunctionDefinition;
    table_argument: string | null;
    session_argument: string | null;
  };
  comment?: string | null;
};

export type IndexType = 'btree' | 'hash' | 'gin' | 'gist' | 'spgist' | 'brin';

export type Index = {
  table_name: string;
  table_schema: string;
  index_name: string;
  index_type: IndexType;
  index_columns: string[];
  index_definition_sql: string;
};

export type IndexFormTips = {
  unique: string;
  indexName: string;
  indexColumns: string;
  indexType: string;
};

export type Schema = {
  schema_name: string;
};

export interface BaseTable {
  table_name: string;
  table_schema: string;
  columns: BaseTableColumn[];
  is_enum?: boolean;
}

export type Constraint = {
  table_name: string;
  table_schema: string;
  constraint_name: string;
  columns: string[];
};

export type PrimaryKey = Constraint;
export type UniqueKey = Constraint;

export type CustomRootFields = {
  select?: Nullable<string> | CustomRootField;
  select_by_pk?: Nullable<string> | CustomRootField;
  select_aggregate?: Nullable<string> | CustomRootField;
  select_stream?: Nullable<string> | CustomRootField;
  insert?: Nullable<string> | CustomRootField;
  insert_one?: Nullable<string> | CustomRootField;
  update?: Nullable<string> | CustomRootField;
  update_by_pk?: Nullable<string> | CustomRootField;
  delete?: Nullable<string> | CustomRootField;
  delete_by_pk?: Nullable<string> | CustomRootField;
  update_many?: Nullable<string> | CustomRootField;
};

export type CustomRootField = {
  name?: Nullable<string>;
  comment?: Nullable<string>;
};

export type ColumnConfig = {
  [column: string]: ColumnConfigValue;
};

export type ColumnConfigValue = {
  custom_name?: Nullable<string>;
  comment?: Nullable<string>;
};

/**
 * @deprecated use {@link NormalizedTable} instead
 * which will be later renamed to Table when it
 * been fully adopted and necessary changes made
 */
export interface Table extends BaseTable {
  table_name: string;
  table_schema: string;
  comment: string | null;
  table_type: TableType;
  primary_key: PrimaryKey | null;
  is_table_tracked: boolean;
  columns: TableColumn[];
  relationships: Relationship[];
  permissions: Permission[];
  foreign_key_constraints: ForeignKeyConstraint[];
  opp_foreign_key_constraints: ForeignKeyConstraint[];
  check_constraints: CheckConstraint[];
  configuration?: {
    column_config: ColumnConfig;
    custom_root_fields: CustomRootFields;
    custom_name: string;
  };
  computed_fields: ComputedField[];
  is_enum: boolean;
  view_info: ViewInfo | null;
  remote_relationships: {
    remote_relationship_name: string;
    table_name: string;
    table_schema: string;
    definition: RemoteRelationshipDef;
  }[];
  citusTableType?: string;
  unique_constraints: UniqueKey[] | null;
  is_apollo_federation_supported?: boolean;
}

export type Partition = {
  parent_schema: string;
  partition_schema: string;
  partition_name: string;
  parent_table: string;
  partition_def: string;
  partition_key: string;
  name?: string;
  isUnique?: boolean;
  isPrimary?: boolean;
};

export type ColumnAction = 'add' | 'modify';

export type DependentSQLGenerator = (
  schemaName: string,
  tableName: string,
  columnName: string
) => { upSql: string; downSql: string };

export interface FrequentlyUsedColumn {
  name: string;
  validFor: ColumnAction[];
  type: Column | string;
  typeText: string;
  primary?: boolean;
  default?: string;
  defaultText?: string;
  dependentSQLGenerator?: DependentSQLGenerator;
  minPGVersion?: number;
}

type ColumnCategories =
  | 'boolean'
  | 'character'
  | 'dateTime'
  | 'geometry'
  | 'geography'
  | 'json'
  | 'jsonb'
  | 'numeric'
  | 'uuid'
  | 'user_defined';
export type PermissionColumnCategories = Record<ColumnCategories, string[]>;

export type SupportedFeaturesType = {
  driver: {
    name: Driver;
    fetchVersion?: {
      enabled: boolean;
    };
  };
  schemas: {
    create: {
      enabled: boolean;
    };
    delete: {
      enabled: boolean;
    };
  };
  tables: {
    create: {
      enabled: boolean;
      frequentlyUsedColumns?: boolean;
      columnTypeSelector?: boolean;
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
      editableTableName?: boolean;
      readOnly?: boolean;
      comments?: {
        view: boolean;
        edit: boolean;
      };
      columns?: {
        view: boolean;
        edit: boolean;
        graphqlFieldName: boolean;
        frequentlyUsedColumns?: boolean;
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
    enabled: boolean;
    track: {
      enabled: boolean;
    };
    nonTrackableFunctions: {
      enabled: boolean;
    };
    modify: {
      enabled: boolean;
      comments: {
        view: boolean;
        edit: boolean;
      };
    };
  };
  events: {
    triggers: {
      enabled: boolean;
      add: boolean;
    };
  };
  actions: {
    enabled: boolean;
    relationships: boolean;
  };
  rawSQL: {
    enabled: boolean;
    statementTimeout: boolean;
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

type Tables = ReduxState['tables'];

export type generateTableRowRequestType = {
  endpoint: string;
  getTableRowRequestBody: (data: {
    tables: Tables;
    isExport?: boolean;
    tableConfiguration: TableConfig;
    dataSourceCustomization: SourceCustomization;
  }) =>
    | {
        type: string;
        source: string;
        args: (
          | ReturnType<typeof getSelectQuery>
          | ReturnType<typeof getRunSqlQuery>
        )[];
      }
    | {
        query: string;
        variables: null;
        operationName: string;
      };
  processTableRowData: <T>(
    data: T,
    config: {
      originalTable: string;
      currentSchema: string;
      tableConfiguration: TableConfig;
      dataSourceCustomization: SourceCustomization;
    }
  ) => { rows: T[]; estimatedCount: number };
};

export type generateInsertRequestType = {
  endpoint: string;
  getInsertRequestBody: (data: {
    tableDef: QualifiedTable;
    source: string;
    insertObject: Record<string, any>;
    tableConfiguration: TableConfig;
    dataSourceCustomization: SourceCustomization;
    returning: string[];
  }) =>
    | {
        type: 'insert';
        args: {
          source: string;
          table: QualifiedTable;
          objects: [Record<string, any>];
          returning: string[];
        };
      }
    | {
        query: string;
        variables: null;
      };
  processInsertData: (
    data:
      | { affectedRows: number; returning: Array<Record<string, any>> }
      | Record<string, Record<string, any>>,
    tableConfiguration: TableConfig,
    dataSourceCustomization: SourceCustomization,
    config: {
      currentTable: string;
      currentSchema: string;
    }
  ) => {
    affectedRows: number | Record<string, any>;
    returnedFields: Record<string, any>;
  };
};

export type GenerateRowsCountRequestType = {
  endpoint: string;
  getRowsCountRequestBody: generateTableRowRequestType['getTableRowRequestBody'];
  processCount: (config: {
    data: any;
    originalTable: string;
    currentSchema: string;
    tableConfiguration: TableConfig;
    dataSourceCustomization: SourceCustomization;
  }) => number;
};

export type GenerateEditRowRequest = {
  endpoint: string;
  processEditData: (args: {
    tableDef: QualifiedTable;
    tableConfiguration: TableConfig;
    dataSourceCustomization: SourceCustomization;
    data: any;
  }) => number;
  getEditRowRequestBody: (data: {
    source: string;
    tableDef: QualifiedTable;
    tableConfiguration: TableConfig;
    dataSourceCustomization: SourceCustomization;
    set: Record<string, any>;
    where: Record<string, any>;
    defaultArray: any[];
  }) =>
    | {
        type: string;
        args: {
          source: string;
          table: QualifiedTable;
          $set: Record<string, any>;
          $default: any[];
          where: Record<string, any>;
        };
      }
    | {
        query: string;
        variables: null;
      };
};

export type RelType = {
  relName: string;
  lTable: string;
  lSchema: string;
  isObjRel: boolean;
  lcol: string[] | null;
  rcol: string[] | null;
  rTable: string | null;
  rSchema: string | null;
};

export type GenerateDeleteRowRequest = {
  endpoint: string;
  getDeleteRowRequestBody: (args: {
    pkClause: WhereClause;
    tableName: string;
    schemaName: string;
    columnInfo: BaseTableColumn[];
    source: string;
    tableConfiguration: TableConfig;
    dataSourceCustomization: SourceCustomization;
  }) =>
    | {
        type: string;
        args: {
          source: string;
          table: {
            name: string;
            schema: string;
          };
          where: WhereClause;
        };
      }
    | {
        query: string;
        variables: null;
      };
  processDeleteRowData: (
    data: Record<string, any>,
    config: {
      dataSourceCustomization: SourceCustomization;
    }
  ) => number;
};

export type GenerateBulkDeleteRowRequest = {
  endpoint: string;
  getBulkDeleteRowRequestBody: (args: {
    pkClauses: WhereClause[];
    tableName: string;
    schemaName: string;
    columnInfo: BaseTableColumn[];
    source: string;
    tableConfiguration: TableConfig;
    dataSourceCustomization: SourceCustomization;
  }) =>
    | {
        type: string;
        source: string;
        args: {
          type: string;
          args: {
            source: string;
            table: {
              name: string;
              schema: string;
            };
            where: WhereClause;
          };
        }[];
      }
    | {
        query: string;
        variables: null;
      };
  processBulkDeleteRowData: (
    data: Record<string, any>,
    config: { dataSourceCustomization: SourceCustomization }
  ) => number;
};
export type ViolationActions =
  | 'restrict'
  | 'no action'
  | 'cascade'
  | 'set null'
  | 'set default';
