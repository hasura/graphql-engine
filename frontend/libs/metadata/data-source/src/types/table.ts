import { Nullable } from '@hasura/shared/types';

export type IndexType = 'btree' | 'hash' | 'gin' | 'gist' | 'spgist' | 'brin';

export type Index = {
  table_name: string;
  table_schema: string;
  index_name: string;
  index_type: IndexType;
  index_columns: string[];
  index_definition_sql: string;
};

export type Constraint = {
  table_name: string;
  table_schema: string;
  constraint_name: string;
  columns: string[];
};

export type PrimaryKey = Constraint;

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
