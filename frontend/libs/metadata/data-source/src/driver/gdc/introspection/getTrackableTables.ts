import { GDCTable } from '@hasura/shared/types';
import { GetTrackableTablesProps } from '../../types';
import { runMetadataQuery } from '@hasura/metadata/api';

export const getTrackableTables = async (props: GetTrackableTablesProps) => {
  const { fetchJson, dataSourceName, endpoints } = props;
  try {
    const result = await runMetadataQuery<GDCTable[]>({
      url: endpoints.metadata,
      fetchJson,
      body: {
        type: 'dataconnector_get_source_tables',
        args: {
          source: dataSourceName,
        },
      },
    });

    const tables = result.map((table) => {
      /**
       * Ideally each table is supposed to be GDCTable, but the server fix has not yet been merged to main.
       * Right now it returns string as a table.
       */
      let name = '';

      if (typeof table === 'string') name = table;
      else if (Array.isArray(table)) name = table.join('.');
      else
        throw Error('getTrackableTables: `table` is not a string or string[]');

      return {
        name,
        table,
        type: 'BASE TABLE',
      };
    });

    return tables;
  } catch (error) {
    throw new Error('Error fetching GDC tables');
  }
};
