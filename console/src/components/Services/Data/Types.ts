export interface TableInfo {
  table_schema: string;
  table_name: string;
}

export interface PrimaryKey extends TableInfo {
  constraint_name: 'article_pkey';
  columns: string[];
}

export interface ManulaRelationshipDef {
  manual_configuration: {
    remote_table: {
      schema: string;
      name: string;
    };
    column_mapping: Record<string, string>;
  };
}

export interface FKRelationshipDef {
  foreign_key_constraint_on: {
    table: {
      schema: string;
      name: string;
    };
    column: string;
  };
}

export interface Relationship extends TableInfo {
  rel_type: 'array' | 'object';
  rel_def: ManulaRelationshipDef | FKRelationshipDef;
  rel_name: string;
  is_system_defined: boolean;
  comment: string | null;
}

export interface Permission {
  filter: Record<string, any>;
  columns: string[];
  computed_fields: string[];
  allow_aggregation: boolean;
}

export interface SchemaPermission extends TableInfo {
  role_name: string;
  permissions: Record<'select' | 'insert' | 'update' | 'delete', Permission>;
}

export interface ForeignKeyConstraint extends TableInfo {
  constraint_name: string;
  constraint_oid: number;
  ref_table_table_schema: string;
  ref_table: string;
  column_mapping: Record<string, string>;
  on_update: string;
  on_delete: string;
  columns: string[];
  ref_columns: string[];
  is_ref_table_tracked: boolean;
  is_unique?: boolean;
}

export interface Schema extends TableInfo {
  table_type: string;
  columns: Array<Record<string, any>>;
  comment: string | null;
  triggers: Array<Record<string, any>>;
  primary_key: PrimaryKey | null;
  relationships: Relationship[];
  permissions: SchemaPermission[];
  unique_constraints: any; // TODO
  check_constraints: any; // TODO
  foreign_key_constraints: ForeignKeyConstraint[];
  opp_foreign_key_constraints: ForeignKeyConstraint[];
  view_info: any; // TODO
  is_enum: boolean;
  configuration: {
    custom_root_fields?: Record<string, any>;
    custom_column_names?: Record<string, any>;
  };
  computed_fields: any; // TODO
}

export type AllSchemas = Schema[];

export type ForeignKey = {
  constraintName: string;
  refSchemaName: string;
  refTableName: string;
  colMappings: [{ column: string; refColumn: string }];
  onUpdate: string;
  onDelete: string;
};

export type FkOptions = {
  from: string;
  to: string;
  displayName: string;
  refTable: string;
};

export type Mappings = {
  columnName: string;
  refTableName: string;
  refColumnName: string;
  displayColumnName: string;
};

export type DisplayConfig = {
  tableName: string;
  schemaName: string;
  constraintName: string;
  mappings: Mappings[];
};

export type ConsoleOpts = {
  telemetryNotificationShown: boolean;
  fkDisplayNames: DisplayConfig[];
};

export type ErrorType = {
  code: string | number;
  message: string;
  error: string;
  internal: {
    error: {
      status_code: string;
      description: string;
      message: string;
    };
  };
};

export type QueryType = {
  type: string;
  args: {
    sql: string;
  }; // todo change this after down schema PR merge
};
