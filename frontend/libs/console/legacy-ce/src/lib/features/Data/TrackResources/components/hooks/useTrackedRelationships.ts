import { useQuery } from 'react-query';
import { tableRelationships as getTableRelationships } from '../../../../DatabaseRelationships/utils/tableRelationships';
import { DataSource } from '../../../../DataSource';
import {
  useMetadata,
  MetadataSelectors,
} from '../../../../hasura-metadata-api';
import { useHttpClient } from '../../../../Network';

export const getTrackedRelationshipsCacheKey = (dataSourceName: string) => [
  'tracked_relationships',
  dataSourceName,
];

export const useTrackedRelationships = (dataSourceName: string) => {
  const httpClient = useHttpClient();

  const {
    data: metadataTables,
    isFetching: isMetadataPending,
    isLoading: isMetadataLoading,
    error: metadataError,
    refetch: refetchMetadata,
  } = useMetadata(MetadataSelectors.getTables(dataSourceName));

  const fetchRelationships = async () => {
    const _tableRelationships = [];
    if (metadataTables && !isMetadataLoading) {
      for (const metadataTable of metadataTables) {
        const fkConstraints = await DataSource(
          httpClient
        ).getTableFkRelationships({
          dataSourceName,
          table: metadataTable.table,
        });

        const tableRelationships = getTableRelationships(
          metadataTable,
          dataSourceName,
          fkConstraints
        );
        _tableRelationships.push(...tableRelationships);
      }
    }

    return _tableRelationships;
  };

  const {
    data: relationships,
    isLoading: isLoadingRelationships,
    isFetching: isFetchingRelationships,
    refetch: refetchRelationships,
  } = useQuery({
    queryFn: fetchRelationships,
    queryKey: getTrackedRelationshipsCacheKey(dataSourceName),
  });

  return {
    data: relationships || [],
    isFetching: isMetadataPending || isFetchingRelationships,
    isLoading: isMetadataLoading || isLoadingRelationships,
    error: [metadataError],
    refetchRelationships,
    refetchMetadata,
  };
};
