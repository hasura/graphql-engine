import { useQuery } from 'react-query';
import { useAllSuggestedRelationships } from '../../../../DatabaseRelationships/components/SuggestedRelationships/hooks/useAllSuggestedRelationships';
import { getTableLocalRelationships } from '../../../../DatabaseRelationships/utils/tableRelationships';
import {
  MetadataSelectors,
  useMetadata,
} from '../../../../hasura-metadata-api';

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
  const { data: currentMetadataSource } = useMetadata(
    MetadataSelectors.findSource(dataSourceName)
  );

  const fetchLocalRelationships = async () => {
    const metadataTables = currentMetadataSource?.tables || [];

    const _tableRelationships = [];
    if (metadataTables) {
      for (const metadataTable of metadataTables) {
        const tableRelationships = getTableLocalRelationships(
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
    queryFn: fetchLocalRelationships,
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
