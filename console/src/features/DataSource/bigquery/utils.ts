import { RunSQLResponse } from '../api';
import { IntrospectedTable } from '../types';

export const adaptIntrospectedBigQueryTables = (
  runSqlResponse: RunSQLResponse
): IntrospectedTable[] => {
  /* 
    The `slice(1)` on the result is done because the first item of the result is always the columns names from the SQL output.
    It is not required for the final result and should be avoided 
  */
  const adaptedResponse = runSqlResponse.result
    ?.slice(1)
    .map((row: string[]) => ({
      name: `${row[1]}.${row[0]}`,
      table: {
        name: row[0],
        dataset: row[1],
      },
      type: row[2],
    }));

  return adaptedResponse ?? [];
};
