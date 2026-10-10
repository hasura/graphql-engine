import type { RunSQLResponse } from '@hasura/shared/types';
import { TableCheckConstraint } from '../../types';
import { sqlResultToRows } from './sqlResultRows';

type CheckConstraintColumn =
  'table_schema' | 'table_name' | 'constraint_name' | 'check';

/**
 * Parse the rows of `checkConstraintsSql` (one per constraint). Pure (no
 * network imports) so it stays cycle-free and unit-testable.
 */
export const adaptCheckConstraints = (
  result: RunSQLResponse['result'] | undefined,
): TableCheckConstraint[] =>
  sqlResultToRows<CheckConstraintColumn>(result)
    .filter((row) => row.constraint_name)
    .map((row) => ({
      name: row.constraint_name as string,
      check: row.check as string,
    }));
