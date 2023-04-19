import { useQuery } from 'react-query';
import { useAllSuggestedRelationships } from '../../../../DatabaseRelationships/components/SuggestedRelationships/hooks/useAllSuggestedRelationships';
import { tableRelationships as getTableRelationships } from '../../../../DatabaseRelationships/utils/tableRelationships';
import { useMetadata } from '../../../../hasura-metadata-api';

export const getTrackedRelationshipsCacheKey = (dataSourceName: string) => [
  'tracked_relationships',
  dataSourceName,
];

export const useTrackedRelationships = (dataSourceName: string) => {
  const { suggestedRelationships } = useAllSuggestedRelationships({
    dataSourceName,
    isEnabled: true,
    omitTracked: false,
  });

  const { data: metadata } = useMetadata(m => m.metadata);

  const fetchRelationships = async () => {
    const currentMetadataSource = metadata?.sources?.find(
      source => source.name === dataSourceName
    );

    const metadataTables = currentMetadataSource?.tables || [];

    const _tableRelationships = [];
    if (metadataTables) {
      for (const metadataTable of metadataTables) {
        const tableRelationships = getTableRelationships(
          metadataTable,
          dataSourceName,
          suggestedRelationships
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
    error,
  } = useQuery({
    queryFn: fetchRelationships,
    queryKey: getTrackedRelationshipsCacheKey(dataSourceName),
  });

  return {
    data: relationships || [],
    isFetching: isFetchingRelationships,
    isLoading: isLoadingRelationships,
    error: [error],
    refetchRelationships,
  };
};
