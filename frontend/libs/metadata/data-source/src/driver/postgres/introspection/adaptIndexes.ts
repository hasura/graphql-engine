import type { RunSQLResponse } from '@hasura/shared/types';
import { TableIndex } from '../../types';
import { parsePostgresTextArray, sqlResultToRows } from './sqlResultRows';

type IndexColumn =
  'index_name' | 'index_type' | 'index_columns' | 'index_definition_sql';

/** Parse the rows of getTableIndexesSql. Pure => cycle-free. */
export const adaptIndexes = (
  result: RunSQLResponse['result'] | undefined,
): TableIndex[] =>
  sqlResultToRows<IndexColumn>(result)
    .filter((row) => row.index_name)
    .map((row) => ({
      name: row.index_name as string,
      type: row.index_type as string,
      columns: parsePostgresTextArray(row.index_columns),
      definition: row.index_definition_sql,
    }));
