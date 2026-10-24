import get from 'lodash/get';
import { runGraphQL } from '@hasura/metadata/api';
import {
  generateGraphQLRowMutation,
  GraphQLRowMutationOperation,
} from '@hasura/shared/utils';
import { Source, Table, Where } from '@hasura/shared/types';
import {
  DataSourceNetworkArgs,
  GetDefaultQueryRootProps,
  TableRow,
} from '../../types';
import { getTableGraphQLContext } from '../getTableGraphQLContext';

export type GetDefaultQueryRoot = (props: GetDefaultQueryRootProps) => string;

type Row = Record<string, unknown>;

/**
 * Inserts, updates or deletes the rows of a table through the GraphQL API,
 * using the live GraphQL schema to resolve the mutation, argument and column
 * names. `objects`, `set`, `where` and the returned rows are keyed by table
 * column names.
 */
export const runRowMutationFromGraphQL = async ({
  endpoints,
  fetchJson,
  source,
  table,
  getDefaultQueryRoot,
  operation,
  objects,
  set,
  where,
  returning,
}: DataSourceNetworkArgs & {
  source: Pick<Source, 'name'>;
  table: Table;
  getDefaultQueryRoot: GetDefaultQueryRoot;
  operation: GraphQLRowMutationOperation;
  objects?: Row[];
  set?: Row;
  where?: Where;
  /** table column names to return */
  returning?: string[];
}): Promise<{ affectedRows: number; returning: TableRow[] }> => {
  // an empty filter would update or delete every row of the table
  if (operation !== 'insert' && !Object.keys(where ?? {}).length)
    throw new Error(`Refusing to ${operation} rows without a filter`);

  const defaultQueryRoot = getDefaultQueryRoot({ table });
  const { schema, tableCustomization, sourceCustomization } =
    await getTableGraphQLContext({
      dataSourceName: source.name,
      table,
      endpoints,
      fetchJson,
      caller: `${operation}Rows`,
    });

  const mutation = generateGraphQLRowMutation({
    schema,
    defaultQueryRoot,
    operation,
    tableCustomization,
    sourceCustomization,
    objects,
    set,
    where,
    returning,
  });

  if (!mutation)
    throw new Error(
      `${operation}Rows: the GraphQL schema exposes no ${operation} mutation for ${defaultQueryRoot}`,
    );

  const data = await runGraphQL({
    url: endpoints.graphQLUrl,
    fetchJson,
    body: {
      operationName: 'TableRowsMutation',
      query: mutation.query,
      variables: mutation.variables,
    },
  });

  const result: Record<string, unknown> = get(data, mutation.resultPath) ?? {};
  const affectedRows = result[mutation.affectedRowsField];
  const rows = mutation.returningField
    ? ((result[mutation.returningField] as Row[] | undefined) ?? [])
    : [];

  return {
    affectedRows: typeof affectedRows === 'number' ? affectedRows : 0,
    returning: rows.map((row) =>
      Object.fromEntries(
        Object.entries(row).map(([field, value]) => [
          mutation.columnNames[field] ?? field,
          value as TableRow[string],
        ]),
      ),
    ),
  };
};
