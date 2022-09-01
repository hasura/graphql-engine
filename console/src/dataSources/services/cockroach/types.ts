import { InputArgType, PostgresTable } from '../postgresql/types';

export interface CockroachTable extends PostgresTable {
  cockroach_table_type: string;
}

export interface FunctionState {
  functionName: string;
  inputArgTypes: InputArgType[];
}
