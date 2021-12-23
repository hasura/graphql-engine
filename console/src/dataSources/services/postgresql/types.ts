// eslint-disable-next-line no-shadow
export enum ArgType {
  CompositeType = 'c',
  BaseType = 'b',
}

export type PGInputArgType = {
  schema: string;
  name: string;
  type: ArgType;
};

export type PGFunction = {
  function_name: string;
  function_schema: string;
  function_definition: string;
  return_type_type: string;
  return_type_name: string;
  function_type: string;
  input_arg_types?: PGInputArgType[];
  returns_set: boolean;
};

export interface PostgresTable {
  table_schema: string;
  table_name: string;
  table_type: string;
  comment: string | null;
  columns: PostgresColumnInfo[];
  view_info: null | PostgresViewInfo;
  triggers: PostgresTrigger[];
}

export interface PostgresViewInfo {
  table_schema: string;
  table_name: string;
  view_definition: string;
  is_updatable: 'NO' | 'YES';
  is_insertable_into: 'NO' | 'YES';
  is_trigger_updatable: 'NO' | 'YES';
  is_trigger_deletable: 'NO' | 'YES';
  is_trigger_insertable_into: 'NO' | 'YES';
}

export interface PostgresColumnInfo {
  comment: string | null;
  data_type_name: string; // 'int4';
  data_type: string; // 'integer';
  table_name: string;
  column_name: string;
  is_nullable: 'NO' | 'YES';
  table_schema: string;
  column_default: string | null;
  ordinal_position: number;
}

export interface PostgresTrigger {
  comment: string | null;
  created: string | null;
  trigger_name: string;
  action_timing: string;
  trigger_schema: string;
  action_statement: string;
  action_orientation: string;
  event_manipulation: string;
}

export type InputArgType = {
  schema: string;
  name: string;
};

export interface FunctionState {
  functionName: string;
  inputArgTypes: InputArgType[];
}
