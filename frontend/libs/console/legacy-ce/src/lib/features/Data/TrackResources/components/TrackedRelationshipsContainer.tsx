import React from 'react';

import {
  MetadataSelectors,
  useMetadata,
  useSyncResourceVersionOnMount,
} from '../../../hasura-metadata-api';
import { TrackedRelationships } from './TrackedRelationships';
import { useTrackedRelationships } from './hooks/useTrackedRelationships';

interface TrackedRelationshipsContainerProps {
  dataSourceName: string;
}

export const TrackedRelationshipsContainer: React.VFC<
  TrackedRelationshipsContainerProps
> = ({ dataSourceName }) => {
  useSyncResourceVersionOnMount({
    componentName: 'TrackedRelationshipsContainer',
  });

  const {
    data: relationships,
    isLoading: isLoadingRelationships,
    refetchRelationships,
  } = useTrackedRelationships(dataSourceName);

  const { data: metadataSource, isLoading: isLoadingMetadata } = useMetadata(
    MetadataSelectors.findSource(dataSourceName)
  );

  return (
    <TrackedRelationships
      dataSourceName={dataSourceName}
      isLoading={isLoadingRelationships || isLoadingMetadata}
      relationships={relationships}
      onUpdate={async () => {
        await refetchRelationships();
      }}
      driver={metadataSource?.kind}
    />
  );
};
