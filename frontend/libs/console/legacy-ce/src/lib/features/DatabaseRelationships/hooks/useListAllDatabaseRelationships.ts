import { Table } from '../../hasura-metadata-types';
import { useMetadata, MetadataSelectors } from '../../hasura-metadata-api';
import { getAllTableRelationships } from '../utils/tableRelationships';
import { useAllSuggestedRelationships } from '../components/SuggestedRelationships/hooks/useAllSuggestedRelationships';

export const useListAllDatabaseRelationships = ({
  dataSourceName,
  table,
}: {
  dataSourceName: string;
  table: Table;
}) => {
  const {
    data: metadataTable,
    isFetching: isMetadataPending,
    isLoading: isMetadataLoading,
    error: metadataError,
  } = useMetadata(MetadataSelectors.findTable(dataSourceName, table));

  const {
    suggestedRelationships,
    isLoadingSuggestedRelationships,
    isFetchingSuggestedRelationships,
    error,
  } = useAllSuggestedRelationships({
    dataSourceName,
    isEnabled: true,
    omitTracked: false,
  });

  return {
    data: getAllTableRelationships(
      metadataTable,
      dataSourceName,
      suggestedRelationships
    ),
    isFetching: isMetadataPending || isFetchingSuggestedRelationships,
    isLoading: isMetadataLoading || isLoadingSuggestedRelationships,
    error: [metadataError, error],
  };
};
