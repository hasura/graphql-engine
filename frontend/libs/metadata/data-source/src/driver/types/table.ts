import { Table } from '@hasura/shared/types';
import { DependentSQLGenerator } from './introspection';
import { ForeignKeyFormSchema, TableFkRelationships } from './relationship';
import { DataSourceNetworkArgs } from './types';

export type CreateTableConstraintArgs = {
  name: string;
  check: string;
};

export type CreateCheckConstraintArgs = {
  table: Table;
  constraintName: string;
  check: string;
};

export type CreateCheckConstraintProps = DataSourceNetworkArgs & {
  dataSourceName: string;
  isMigration: boolean;
} & CreateCheckConstraintArgs;

export type DropCheckConstraintArgs = {
  table: Table;
  constraintName: string;
  // Optional CHECK expression, used to build the down migration when known.
  check?: string;
};

export type DropCheckConstraintProps = DataSourceNetworkArgs & {
  dataSourceName: string;
  isMigration: boolean;
} & DropCheckConstraintArgs;

// ---- Primary keys ----
export type CreatePrimaryKeyArgs = {
  table: Table;
  constraintName: string;
  columns: string[];
};
export type CreatePrimaryKeyProps = DataSourceNetworkArgs & {
  dataSourceName: string;
  isMigration: boolean;
} & CreatePrimaryKeyArgs;

export type AlterPrimaryKeyArgs = {
  table: Table;
  constraintName: string;
  columns: string[];
  // Previous columns, used to build the down migration when known.
  previousColumns?: string[];
};
export type AlterPrimaryKeyProps = DataSourceNetworkArgs & {
  dataSourceName: string;
  isMigration: boolean;
} & AlterPrimaryKeyArgs;

export type DropPrimaryKeyArgs = {
  table: Table;
  constraintName: string;
  // Columns, used to rebuild the PK on the down migration when known.
  columns?: string[];
};
export type DropPrimaryKeyProps = DataSourceNetworkArgs & {
  dataSourceName: string;
  isMigration: boolean;
} & DropPrimaryKeyArgs;

// ---- Unique keys ----
export type CreateUniqueKeyArgs = {
  table: Table;
  constraintName: string;
  columns: string[];
};
export type CreateUniqueKeyProps = DataSourceNetworkArgs & {
  dataSourceName: string;
  isMigration: boolean;
} & CreateUniqueKeyArgs;

export type DropUniqueKeyArgs = {
  table: Table;
  constraintName: string;
  // Columns, used to rebuild the unique key on the down migration when known.
  columns?: string[];
};
export type DropUniqueKeyProps = DataSourceNetworkArgs & {
  dataSourceName: string;
  isMigration: boolean;
} & DropUniqueKeyArgs;

// ---- Columns ----
export type AlterColumnDefinition = {
  name: string;
  /** SQL type, e.g. `integer` or `character varying(255)`. */
  type: string;
  nullable: boolean;
  /** Raw SQL default expression; `null` (or empty) means no default. */
  default: string | null;
  unique: boolean;
};

export type AlterColumnArgs = {
  table: Table;
  /** The column as it currently is in the database. */
  previous: AlterColumnDefinition & {
    /** Name of the existing single-column unique constraint, if any. */
    uniqueConstraintName?: string;
  };
  next: AlterColumnDefinition;
};
export type AlterColumnProps = DataSourceNetworkArgs & {
  dataSourceName: string;
  isMigration: boolean;
} & AlterColumnArgs;

export type ModifyTableColumnArgs = {
  name: string;
  type: string;
  nullable?: boolean;
  default?: {
    value: unknown;
  };
  dependentSQLGenerator?: DependentSQLGenerator;
};

export type DropColumnArgs = {
  table: Table;
  /** The column as it currently is, used to re-add it on the down migration. */
  column: Omit<AlterColumnDefinition, 'unique'>;
  /** Constraints involving the column. Postgres drops them along with it, so
   *  the down migration recreates them. */
  primaryKey?: { constraintName: string; columns: string[] };
  uniqueKeys?: { constraintName: string; columns: string[] }[];
  foreignKeys?: TableFkRelationships[];
};
export type DropColumnProps = DataSourceNetworkArgs & {
  dataSourceName: string;
  isMigration: boolean;
} & DropColumnArgs;

export type AddColumnArgs = {
  table: Table;
  column: ModifyTableColumnArgs & { unique?: boolean };
};
export type AddColumnProps = DataSourceNetworkArgs & {
  dataSourceName: string;
  isMigration: boolean;
} & AddColumnArgs;

export type ModifyTableArgs = {
  table: Table;
  columns: ModifyTableColumnArgs[];
  primaryKeys: number[];
  foreignKeys: ForeignKeyFormSchema[];
  uniqueKeys: number[][];
  checkConstraints: CreateTableConstraintArgs[];
  /** Stored as the tracked table's metadata `configuration.comment`, not as a
   *  SQL `COMMENT ON TABLE`. */
  tableComment?: string;
};

export type ModifyTableProps = DataSourceNetworkArgs & {
  dataSourceName: string;
  isMigration: boolean;
  args: ModifyTableArgs;
};

// ---- Indexes ----
export type PostgresIndexTypeArg =
  'btree' | 'hash' | 'gin' | 'gist' | 'spgist' | 'brin';

export type CreateIndexArgs = {
  table: Table;
  indexName: string;
  indexType: PostgresIndexTypeArg;
  columns: string[];
  unique?: boolean;
};
export type CreateIndexProps = DataSourceNetworkArgs & {
  dataSourceName: string;
  isMigration: boolean;
} & CreateIndexArgs;

export type DropIndexArgs = {
  table: Table;
  indexName: string;
};
export type DropIndexProps = DataSourceNetworkArgs & {
  dataSourceName: string;
  isMigration: boolean;
} & DropIndexArgs;

// ---- Triggers (list + drop; create is out of scope, matching legacy) ----
export type TriggerTiming = 'BEFORE' | 'AFTER';
export type TriggerEvent = 'INSERT' | 'UPDATE' | 'DELETE' | 'TRUNCATE';

export type CreateTriggerArgs = {
  table: Table;
  triggerName: string;
  timing: TriggerTiming;
  events: TriggerEvent[];
  forEach: 'ROW' | 'STATEMENT';
  /** Optional `WHEN (...)` condition, without the parentheses. */
  condition?: string;
  function: { schema: string; name: string };
  /** When set, the trigger function is created (plpgsql) with this body and
   *  dropped again on the down migration. */
  newFunctionBody?: string;
};
export type CreateTriggerProps = DataSourceNetworkArgs & {
  dataSourceName: string;
  isMigration: boolean;
} & CreateTriggerArgs;

export type DropTriggerArgs = {
  table: Table;
  triggerName: string;
};
export type DropTriggerProps = DataSourceNetworkArgs & {
  dataSourceName: string;
  isMigration: boolean;
} & DropTriggerArgs;
