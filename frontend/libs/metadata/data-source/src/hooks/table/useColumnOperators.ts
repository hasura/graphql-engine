import { useQuery } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource, Table } from '@hasura/shared/types';
import {
  ComparisonOperator,
  getColumnComparisonOperators,
} from '@hasura/shared/utils';
import { defaultQueryOptions } from '@hasura/metadata/api';
import { getDatabaseMethods, Operator } from '../../driver';
import { getTableGraphQLContext } from '../../driver/common/getTableGraphQLContext';
import { useTableColumns } from '../introspection';

const COLUMN_OPERATORS_QUERY_KEY = 'COLUMN_OPERATORS';

/**
 * Names an operator after the driver's label for it, e.g. `_ilike` ->
 * `like (case-insensitive)`, or after its own name, e.g. `_has_key` ->
 * `has key`, with the type of its operand.
 */
const toOperator = (
  { name, type, isList }: ComparisonOperator,
  driverOperators: Operator[],
): Operator => ({
  ...(driverOperators.find((operator) => operator.value === name) ?? {
    name: name.replace(/^_/, '').replace(/_/g, ' '),
    value: name,
  }),
  valueType: type,
  isListValue: isList,
});

/**
 * Returns the filter operators each column supports, keyed by column name.
 * They are read from the GraphQL schema, so they depend on the column type
 * (e.g. no `_like` on numbers) and include driver specific operators (e.g.
 * `_contains` on `jsonb`).
 */
export const useColumnOperators = ({
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
    queryKey: [source.name, table, COLUMN_OPERATORS_QUERY_KEY, columns],
    queryFn: async () => {
      const { schema, tableCustomization, sourceCustomization } =
        await getTableGraphQLContext({
          dataSourceName: source.name,
          table,
          endpoints,
          fetchJson,
          caller: 'getColumnOperators',
        });

      const dataSource = getDatabaseMethods(source.kind);
      const driverOperators = dataSource.introspection.getSupportedOperators();
      const operators = getColumnComparisonOperators({
        schema,
        defaultQueryRoot: dataSource.config.getDefaultQueryRoot({ table }),
        columns,
        tableCustomization,
        sourceCustomization,
      });

      return Object.fromEntries(
        Object.entries(operators).map(([column, columnOperators]) => [
          column,
          columnOperators.map((operator) =>
            toOperator(operator, driverOperators),
          ),
        ]),
      ) as Record<string, Operator[]>;
    },
    enabled: columns.length > 0,
    ...defaultQueryOptions,
  });
};
