import { GDCTable } from '..';
import { runMetadataQuery } from '../../api';
import { GetTableColumnsProps, TableColumn } from '../../types';

/**
 * Refer - https://github.com/hasura/graphql-engine-mono/blob/main/dc-agents/dc-api-types/src/models/TableInfo.ts
 */
type TableInfoResponseType = {
  name: GDCTable;
  columns: { name: string; type: string; nullable: string }[];
  primary_key?: string[] | null;
  description?: string;
  foreign_keys?: Record<
    string,
    {
      foreign_table: GDCTable;
      column_mapping: Record<string, string>;
    }
  >;
};

export const getTableColumns = async (props: GetTableColumnsProps) => {
  const { httpClient, dataSourceName, table } = props;

  try {
    const result = await runMetadataQuery<TableInfoResponseType>({
      httpClient,
      body: {
        type: 'get_table_info',
        args: {
          source: dataSourceName,
          table,
        },
      },
    });

    return result.columns.map<TableColumn>(column => ({
      name: column.name,
      dataType: column.type,
    }));
  } catch (error) {
    throw new Error('Error fetching GDC columns');
  }
};
