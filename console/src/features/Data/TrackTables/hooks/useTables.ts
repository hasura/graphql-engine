import {
  getTableName,
  IntrospectedTable,
  DataSource,
  exportMetadata,
  Feature,
} from '@/features/DataSource';
import { MetadataTable } from '@/features/MetadataAPI';
import { useHttpClient } from '@/features/Network';
import { useQuery } from 'react-query';
import type { TrackableTable } from '../types';

export type UseTablesProps = {
  dataSourceName: string;
};

const getTrackableTables = (
  trackedTables: MetadataTable[],
  introspectedTables: IntrospectedTable[],
  databaseHierarchy: string[]
) =>
  introspectedTables.map(introspectedTable => {
    const trackedTable = trackedTables.find(
      _trackedTable =>
        getTableName(_trackedTable.table, databaseHierarchy) ===
        introspectedTable.name
    );

    const isTracked = !!trackedTable;
    if (isTracked) {
      const trackableTable: TrackableTable = {
        id: introspectedTable.name,
        name: introspectedTable.name,
        table: introspectedTable.table,
        type: introspectedTable.type,
        is_tracked: isTracked,
        configuration: {
          custom_root_fields: trackedTable?.configuration?.custom_root_fields,
        },
      };
      return trackableTable;
    }

    const trackableTable: TrackableTable = {
      id: introspectedTable.name,
      name: introspectedTable.name,
      table: introspectedTable.table,
      type: introspectedTable.type,
      is_tracked: isTracked,
    };
    return trackableTable;
  });

// adding this export so if the key changes outside code won't get out of sync
export const tablesQueryKey = (dataSourceName: string) => [
  'introspected-tables',
  dataSourceName,
];

export const useTables = ({ dataSourceName }: UseTablesProps) => {
  const httpClient = useHttpClient();
  return useQuery<TrackableTable[], Error>({
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

      const introspectedTables = await DataSource(httpClient).introspectTables({
        dataSourceName,
      });

      if (introspectedTables === Feature.NotImplemented)
        throw Error(
          `useTables.introspectedTables Feature is not available for ${currentMetadataSource.kind}`
        );

      const trackedTables = currentMetadataSource.tables;
      const databaseHierarchy = await DataSource(
        httpClient
      ).getDatabaseHierarchy({ dataSourceName });

      const trackableTables = getTrackableTables(
        trackedTables,
        introspectedTables,
        databaseHierarchy
      );
      return trackableTables;
    },
    refetchOnWindowFocus: false,
  });
};
