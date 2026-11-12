import { QueryClient, useQuery } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource, Table } from '@hasura/shared/types';
import { getEnumColumnValues } from '@hasura/shared/utils';
import { defaultQueryOptions } from '@hasura/metadata/api';
import { getDatabaseMethods } from '../../driver';
import { getTableGraphQLContext } from '../../driver/common/getTableGraphQLContext';
import { useTableColumns } from '../introspection';

const ENUM_COLUMN_VALUES_QUERY_KEY = 'ENUM_COLUMN_VALUES';

/** Refetches the enum values of every column of a source, e.g. after a reload. */
export const invalidateEnumColumnValues = (
  queryClient: QueryClient,
  sourceName: string,
) =>
  queryClient.invalidateQueries({
    predicate: ({ queryKey }) =>
      queryKey[0] === sourceName &&
      queryKey[2] === ENUM_COLUMN_VALUES_QUERY_KEY,
  });

/**
 * Returns the allowed values of the columns that reference an enum table,
 * keyed by column name. They are read from the GraphQL enum types, so they
 * match what the API returns and accepts under any naming convention (e.g.
 * `graphql-default` upper-cases them).
 */
export const useEnumColumnValues = ({
  source,
  table,
}: {
  source: QualifiedDataSource;
  table: Table;
}) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const { data: columns = [] } = useTableColumns(
    { source, table },
    { select: (result) => result.columns.map((column) => column.name) },
  );

  return useQuery({
    queryKey: [source.name, table, ENUM_COLUMN_VALUES_QUERY_KEY, columns],
    queryFn: async () => {
      const { schema, tableCustomization, sourceCustomization } =
        await getTableGraphQLContext({
          dataSourceName: source.name,
          table,
          endpoints,
          fetchJson,
          caller: 'getEnumColumnValues',
        });

      return getEnumColumnValues({
        schema,
        defaultQueryRoot: getDatabaseMethods(
          source.kind,
        ).config.getDefaultQueryRoot({ table }),
        columns,
        tableCustomization,
        sourceCustomization,
      });
    },
    enabled: columns.length > 0,
    ...defaultQueryOptions,
  });
};
