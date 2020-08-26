import { Nullable } from '../components/Common/utils/tsUtils';
import { FunctionDefinition } from '../components/Common/utils/v1QueryUtils';

export type Relationship = {
  rel_name: string;
  rel_def: {
    manual_configuration?: any;
    foreign_key_constraint_on?: any;
  };
  rel_type: 'object' | 'array';
};

export type Permission = {
  role_name: string;
  permissions: {
    [action: string]: any;
  };
};

export interface BaseTableColumn {
  column_name: string;
  data_type: string;
}

export interface TableColumn extends BaseTableColumn {
  udt_name: string;
  column_default: string;
  is_generated?: boolean;
  is_nullable?: string;
  is_identity?: boolean;
  identity_generation?: 'ALWAYS' | 'BY DEFAULT' | null;
}

export type ForeignKeyConstraint = {
  ref_table: string;
  ref_table_table_schema: string;
  column_mapping: {
    [lcol: string]: string;
  };
};

export type CheckConstraint = {
  constraint_name: string;
  check: string;
};

export type ComputedField = {
  computed_field_name: string;
  definition: {
    function: FunctionDefinition;
  };
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
    | 'PARTITIONED TABLE';
  is_table_tracked: boolean;
  columns: TableColumn[];
  relationships: Relationship[];
  permissions: Permission[];
  foreign_key_constraints: ForeignKeyConstraint[];
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
  };
  computed_fields: ComputedField[];
  is_enum: boolean;
  view_info: {
    is_trigger_insertable_into: 'YES' | 'NO';
    is_insertable_into: 'YES' | 'NO';
    is_updatable: 'YES' | 'NO';
    is_trigger_updatable: 'YES' | 'NO';
    is_trigger_deletable: 'YES' | 'NO';
  };
}
