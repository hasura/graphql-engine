import { Nullable } from '@hasura/shared/types';

export type TableType =
  | 'TABLE'
  | 'VIEW'
  | 'MATERIALIZED VIEW'
  | 'FOREIGN TABLE'
  | 'PARTITIONED TABLE'
  | 'BASE TABLE'
  | 'EXTERNAL'; // specific to Big Query

export type CheckConstraint = {
  table_schema: string;
  table_name: string;
  constraint_name: string;
  column_name?: string | null;
  /** sql definition */
  check: string;
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

export interface PostgresTrigger {
  comment: string | null;
  created: string | null;
  trigger_name: string;
  action_timing: string;
  trigger_schema: string;
  action_statement: string;
  action_orientation: string;
  action_condition: string;
  event_manipulation: string;
}

export type CustomRootField = {
  name?: Nullable<string>;
  comment?: Nullable<string>;
};
