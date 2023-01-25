import { runQuery } from '../../api';
import { GetTableRowsProps, TableRow } from '../../types';

export const getTableRows = async (props: GetTableRowsProps) => {
  const { httpClient, dataSourceName, table, columns, options } = props;
  /**
   * For postgres, fetching row data for tables is done via RQL query. The `getSelectQuery` method generates the RQL query for
   * pg table given columns and other options like where, sort, offset, limit & order_by.
   * This eventually has to be moved to a GQL implemenation but GQL has some limitations, specifically we cannot reliably generate the graphql query root
   * names because of customization settings available to datasources as well as tables. Once we have an API to get the GQL introspection names, we can
   * do it via GQL.
   */

  const result = await runQuery<TableRow[]>({
    body: {
      type: 'select',
      args: {
        source: dataSourceName,
        table,
        columns,
        where: options?.where ? { $and: options.where } : undefined,
        offset: options?.offset,
        limit: options?.limit,
        order_by: options?.order_by,
      },
    },
    httpClient,
  });

  return result;
};
