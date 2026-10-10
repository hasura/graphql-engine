import { runMetadataQuery } from '@hasura/metadata/api';
import { GetFKRelationshipProps, TableFkRelationships } from '../../types';
import { GetTableInfoResponse } from './types';

export const getFKRelationships: (
  props: GetFKRelationshipProps,
) => Promise<TableFkRelationships[]> = async (props) => {
  const { fetchJson, dataSourceName, table, endpoints } = props;

  try {
    const tableInfo = await runMetadataQuery<GetTableInfoResponse>({
      url: endpoints.metadata,
      fetchJson,
      body: {
        type: 'get_table_info',
        args: {
          source: dataSourceName,
          table,
        },
      },
    });

    if (!tableInfo.foreign_keys) {
      return [];
    }

    return Object.entries(tableInfo.foreign_keys).map(([, foreignKey]) => {
      const fromColumns = Object.keys(foreignKey.column_mapping);
      const toColumns = Object.values(foreignKey.column_mapping);
      return {
        from: { columns: fromColumns, table: props.table },
        to: { columns: toColumns, table: foreignKey.foreign_table },
      };
    });
  } catch (error) {
    console.error(error);
    throw new Error('Error fetching GDC foreign keys');
  }
};
