import React, { useEffect } from 'react';
import { useTrackedRelationships } from './hooks/useTrackedRelationships';
import { useInvalidateMetadata } from '../../../hasura-metadata-api';
import { useMetadata } from '../../../MetadataAPI';
import { TrackedRelationships } from './TrackedRelationships';

const useInvalidateMetadataOnLoad = () => {
  const invalidateMetadata = useInvalidateMetadata();

  // just invalidate metadata when this screen loads for the first time
  // why? because the user might be coming from a redux based paged and the resource_version might gone out of sync
  useEffect(() => {
    invalidateMetadata();
  }, [invalidateMetadata]);
};

interface TrackedRelationshipsContainerProps {
  dataSourceName: string;
}

export const TrackedRelationshipsContainer: React.VFC<
  TrackedRelationshipsContainerProps
> = ({ dataSourceName }) => {
  useInvalidateMetadataOnLoad();

  const {
    data: relationships,
    isLoading: isLoadingRelationships,
    refetchRelationships,
  } = useTrackedRelationships(dataSourceName);

  const {
    data: metadataDataSource,
    refetch: refetchMetadata,
    isLoading: isLoadingMetadata,
  } = useMetadata(m => {
    return {
      resource_version: m.resource_version,
      source: m.metadata.sources.find(s => s.name === dataSourceName),
    };
  });
  const metadataSource = metadataDataSource?.source;
  const driver = metadataSource?.kind;

  return (
    <TrackedRelationships
      dataSourceName={dataSourceName}
      isLoading={isLoadingRelationships || isLoadingMetadata}
      relationships={relationships}
      onUpdate={async () => {
        await refetchRelationships();
        await refetchMetadata();
      }}
      driver={driver}
    />
  );
};
