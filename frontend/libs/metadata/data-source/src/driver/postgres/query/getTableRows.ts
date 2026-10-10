import { Where } from '@hasura/shared/types';
import { GetTableRowsProps } from '../../types';

export const getTableRows = async (props: GetTableRowsProps) => {
  const { dataSourceName, table, columns, options, fetchJson, endpoints } =
    props;
  /**
   * For postgres, fetching row data for tables is done via RQL query. The `getSelectQuery` method generates the RQL query for
   * pg table given columns and other options like where, sort, offset, limit & order_by.
   * This eventually has to be moved to a GQL implementation but GQL has some limitations, specifically we cannot reliably generate the graphql query root
   * names because of customization settings available to datasources as well as tables. Once we have an API to get the GQL introspection names, we can
   * do it via GQL.
   */
  // FIXME: /v1/query or /v2/query?
  const result = await fetchJson(endpoints.queryV2, {
    method: 'POST',
    body: JSON.stringify({
      type: 'select',
      args: {
        source: dataSourceName,
        table,
        columns: columns.map((col) => col.name),
        where: options?.where ? buildWhere(options.where) : undefined,
        offset: options?.offset,
        limit: options?.limit,
        order_by: options?.order_by,
      },
    }),
  });

  return result;
};

const buildWhere = (where: Where) => {
  if ('_and' in where) {
    return {
      $and: where._and,
    };
  }

  if ('_or' in where) {
    return {
      $or: where._or,
    };
  }

  return where;
};
