import { useQuery } from 'react-query';
import {
  DataSource,
  exportMetadata,
  TableColumn,
} from '../../../../../DataSource';
import { MetadataTable } from '../../../../../hasura-metadata-types';
import { useHttpClient } from '../../../../../Network';

const tablesQueryKey = (dataSourceName: string) => [
  'tables-with-columns',
  dataSourceName,
];

export type TableWithColumns = {
  metadataTable: MetadataTable;
  columns: TableColumn[];
};

export const useTablesWithColumns = ({
  dataSourceName,
}: {
  dataSourceName: string;
}) => {
  const httpClient = useHttpClient();
  return useQuery<TableWithColumns[], Error>({
    queryKey: tablesQueryKey(dataSourceName),
    queryFn: async () => {
      const { metadata } = await exportMetadata({
        httpClient,
      });

      if (!metadata) throw Error('metadata not found');

      const currentMetadataSource = metadata.sources?.find(
        source => source.name === dataSourceName
      );

      if (!currentMetadataSource)
        throw Error(`useTables.metadataSource not found`);

      const result: TableWithColumns[] = [];

      for (const metadataTable of currentMetadataSource.tables) {
        const columns = await DataSource(httpClient).getTableColumns({
          dataSourceName,
          table: metadataTable.table,
        });
        result.push({ metadataTable, columns });
      }

      return result;
    },
    refetchOnWindowFocus: false,
  });
};
