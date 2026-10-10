import { Table } from '@hasura/shared/types';
import { DataSourceNetworkArgs } from './types';

export type ModifyTableAction = 'add' | 'modify';

export type TableCheckConstraint = {
  name: string;
  check: string;
};

export type GetCheckConstraintsProps = DataSourceNetworkArgs & {
  dataSourceName: string;
  table: Table;
};

export type TableKeyConstraint = {
  constraintName: string;
  columns: string[];
};

export type GetTableKeysProps = DataSourceNetworkArgs & {
  dataSourceName: string;
  table: Table;
};

export type TableIndex = {
  name: string;
  type: string;
  columns: string[];
  definition?: string;
};

export type GetTableIndexesProps = DataSourceNetworkArgs & {
  dataSourceName: string;
  table: Table;
};

export type TableTrigger = {
  name: string;
  timing?: string;
  events?: string;
  /** The trigger's action, e.g. `EXECUTE FUNCTION "public"."fn"()`. */
  definition?: string;
  /** The full `CREATE TRIGGER` statement, when the driver can introspect it. */
  createStatement?: string;
};

/** A function that returns `trigger`, usable in `CREATE TRIGGER`. */
export type TriggerFunction = { schema: string; name: string };

export type GetTriggerFunctionsProps = DataSourceNetworkArgs & {
  dataSourceName: string;
};

export type GetTableTriggersProps = DataSourceNetworkArgs & {
  dataSourceName: string;
  table: Table;
};

export type DependentSQLGeneratorResult = { upSql: string; downSql: string };

export type DependentSQLGenerator = (
  table: Table,
  columnName: string,
) => DependentSQLGeneratorResult;

export type FrequentlyUsedColumn = {
  name: string;
  validFor: ModifyTableAction[];
  type: string;
  typeText: string;
  primary?: boolean;
  default?: string;
  defaultText?: string;
  dependentSQLGenerator?: DependentSQLGenerator;
  minPGVersion?: number;
};
