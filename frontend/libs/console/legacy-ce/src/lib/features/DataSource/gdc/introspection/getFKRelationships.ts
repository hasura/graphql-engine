/* eslint-disable no-underscore-dangle */
import { runMetadataQuery } from '../../api';
import { GetFKRelationshipProps, TableFkRelationships } from '../../types';
import { GetTableInfoResponse } from './types';

export const getFKRelationships: (
  props: GetFKRelationshipProps
) => Promise<TableFkRelationships[]> = async props => {
  const { httpClient, dataSourceName, table } = props;

  try {
    const tableInfo = await runMetadataQuery<GetTableInfoResponse>({
      httpClient,
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
        from: { column: fromColumns, table: props.table },
        to: { column: toColumns, table: foreignKey.foreign_table },
      };
    });
  } catch (error) {
    console.error(error);
    throw new Error('Error fetching GDC foreign keys');
  }
};
