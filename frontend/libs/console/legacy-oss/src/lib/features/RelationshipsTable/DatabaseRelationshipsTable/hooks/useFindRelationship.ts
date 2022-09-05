import { exportMetadata, Table } from '@/features/DataSource';
import { useHttpClient } from '@/features/Network';
import { useQuery } from 'react-query';
import { areTablesEqual } from '../../utils';

export const useFindRelationship = ({
  dataSourceName,
  relationshipName,
  table,
}: {
  dataSourceName: string;
  table: Table;
  relationshipName: string;
}) => {
  const httpClient = useHttpClient();
  return useQuery({
    queryKey: ['get_existing_relationship', dataSourceName, relationshipName],
    queryFn: async () => {
      const { metadata } = await exportMetadata({ httpClient });
      const metadataSource = metadata.sources.find(
        source => source.name === dataSourceName
      );

      if (!metadataSource) throw Error('source not found');

      const metadataTable = metadataSource.tables.find(t =>
        areTablesEqual(table, t.table)
      );
      if (!metadataTable) throw Error('table not found');

      /**
       * Look for the relationship inside array_relationship, object_relationships & remote_relationships
       */

      const allRelationships = [
        ...(metadataTable.object_relationships ?? []),
        ...(metadataTable.array_relationships ?? []),
        ...(metadataTable.remote_relationships ?? []),
      ];

      const relationship = allRelationships.find(
        rel => rel.name === relationshipName
      );

      return relationship;
    },
  });
};
