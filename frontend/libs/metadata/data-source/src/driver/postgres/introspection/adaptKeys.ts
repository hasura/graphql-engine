import type { RunSQLResponse } from '@hasura/shared/types';
import { TableKeyConstraint } from '../../types';
import { parsePostgresTextArray, sqlResultToRows } from './sqlResultRows';

type KeyColumn = 'table_name' | 'table_schema' | 'constraint_name' | 'columns';

/**
 * Parse the rows of `getKeysSql` into `{ constraintName, columns }[]`.
 * Pure (no network imports) => cycle-free and unit-testable.
 */
export const adaptKeyConstraints = (
  result: RunSQLResponse['result'] | undefined,
): TableKeyConstraint[] =>
  sqlResultToRows<KeyColumn>(result)
    .filter((row) => row.constraint_name)
    .map((row) => ({
      constraintName: row.constraint_name as string,
      columns: parsePostgresTextArray(row.columns),
    }));
