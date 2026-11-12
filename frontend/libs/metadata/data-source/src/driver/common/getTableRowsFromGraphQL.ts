import get from 'lodash/get';
import { runGraphQL } from '@hasura/metadata/api';
import { generateGraphQLSelectQuery } from '@hasura/shared/utils';
import { GetTableRowsProps, TableRow } from '../types';
import { getTableGraphQLContext } from './getTableGraphQLContext';

/**
 * Fetches the rows of a table through the GraphQL API, using the live GraphQL
 * schema to resolve the root field and column names. The rows are keyed by
 * the table column names.
 */
export const getTableRowsFromGraphQL = async ({
  table,
  dataSourceName,
  columns,
  options,
  endpoints,
  fetchJson,
  defaultQueryRoot,
  metadata,
}: GetTableRowsProps & { defaultQueryRoot: string }): Promise<TableRow[]> => {
  const { schema, tableCustomization, sourceCustomization } =
    await getTableGraphQLContext({
      dataSourceName,
      table,
      endpoints,
      fetchJson,
      caller: 'getTableRows',
      metadata,
    });

  const selectQuery = generateGraphQLSelectQuery({
    schema,
    defaultQueryRoot,
    columns: columns.map((column) => column.name),
    tableCustomization,
    sourceCustomization,
    options,
  });

  if (!selectQuery)
    throw new Error(
      `getTableRows: the GraphQL schema exposes no select query or columns for ${defaultQueryRoot}`,
    );

  const data = await runGraphQL({
    url: endpoints.graphQLUrl,
    fetchJson,
    body: {
      operationName: 'TableRows',
      query: selectQuery.query,
      variables: selectQuery.variables,
    },
  });

  const rows: Record<string, TableRow[string]>[] =
    get(data, selectQuery.resultPath) ?? [];

  return rows.map((row) =>
    Object.fromEntries(
      Object.entries(row).map(([field, value]) => [
        selectQuery.columnNames[field] ?? field,
        value,
      ]),
    ),
  );
};
