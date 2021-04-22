import { Nullable } from '../components/Common/utils/tsUtils';
import { Column } from '../utils/postgresColumnTypes';
import {
  FunctionDefinition,
  RemoteRelationshipDef,
  TableEntry,
} from '../metadata/types';
import { ReduxState } from '../types';
import {
  getSelectQuery,
  getRunSqlQuery,
} from '../../src/components/Common/utils/v1QueryUtils';

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

export interface TableColumn extends BaseTableColumn {
  is_generated?: boolean;
  is_nullable?: string;
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
  constraint_name: string;
  check: string;
};

export type ComputedField = {
  computed_field_name: string;
  definition: {
    function: FunctionDefinition;
    table_argument: string | null;
    session_argument: string | null;
  };
  comment: string | null;
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

export interface Table extends BaseTable {
  table_name: string;
  table_schema: string;
  table_type:
    | 'TABLE'
    | 'VIEW'
    | 'MATERIALIZED VIEW'
    | 'FOREIGN TABLE'
    | 'PARTITIONED TABLE'
    | 'BASE TABLE'
    | 'TABLE' // specific to SQL Server
    | 'EXTERNAL'; // specific to Big Query
  primary_key: {
    table_name: string;
    table_schema: string;
    constraint_name: string;
    columns: string[];
  } | null;
  is_table_tracked: boolean;
  columns: TableColumn[];
  relationships: Relationship[];
  permissions: Permission[];
  foreign_key_constraints: ForeignKeyConstraint[];
  opp_foreign_key_constraints: ForeignKeyConstraint[];
  check_constraints: CheckConstraint[];
  configuration?: {
    custom_column_names: {
      [column: string]: string;
    };
    custom_root_fields: {
      select: Nullable<string>;
      select_by_pk: Nullable<string>;
      select_aggregate?: Nullable<string>;
      insert?: Nullable<string>;
      insert_one?: Nullable<string>;
      update?: Nullable<string>;
      update_by_pk?: Nullable<string>;
      delete?: Nullable<string>;
      delete_by_pk?: Nullable<string>;
    };
    custom_name: string;
  };
  computed_fields: ComputedField[];
  is_enum: boolean;
  view_info: {
    is_trigger_insertable_into: 'YES' | 'NO';
    is_insertable_into: 'YES' | 'NO';
    is_updatable: 'YES' | 'NO';
    is_trigger_updatable: 'YES' | 'NO';
    is_trigger_deletable: 'YES' | 'NO';
    table_schema: string;
    table_name: string;
    view_definition: string;
  } | null;
  remote_relationships: {
    remote_relationship_name: string;
    table_name: string;
    table_schema: string;
    definition: RemoteRelationshipDef;
  }[];
}

export type ColumnAction = 'add' | 'modify';

export interface FrequentlyUsedColumn {
  name: string;
  validFor: ColumnAction[];
  type: Column | string;
  typeText: string;
  primary?: boolean;
  default?: string;
  defaultText?: string;
  dependentSQLGenerator?: (
    schemaName: string,
    tableName: string,
    columnName: string
  ) => { upSql: string; downSql: string };
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
    name: string;
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
    };
    browse: {
      enabled: boolean;
      customPagination?: boolean;
      aggregation: boolean;
    };
    insert: {
      enabled: boolean;
    };
    modify: {
      enabled: boolean;
    };
    relationships: {
      enabled: boolean;
      remoteRelationships?: boolean;
      track: boolean;
    };
    permissions: {
      enabled: boolean;
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
    tracking: boolean;
  };
  connectDbForm: {
    connectionParameters: boolean;
    databaseURL: boolean;
    environmentVariable: boolean;
    read_replicas: boolean;
  };
};

type Tables = ReduxState['tables'];

export type generateTableRowRequestType = {
  endpoint: string;
  getTableRowRequestBody: (data: {
    tables: Tables;
    isExport?: boolean;
    tableConfiguration?: TableEntry['configuration'];
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
    config?: {
      originalTable: string;
      currentSchema: string;
      tableConfiguration?: TableEntry['configuration'];
    }
  ) => { rows: T[]; estimatedCount: number };
};
