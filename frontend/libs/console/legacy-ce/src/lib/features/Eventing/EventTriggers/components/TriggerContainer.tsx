import React from 'react';
import { Outlet, useParams } from 'react-router';
import { useMetadata } from '@hasura/metadata/api';
import { MetadataSelectors } from '@hasura/metadata/helpers';

import { EventTriggerDetailContext } from '../context';
import { IndicatorCard, SkeletonList } from '@hasura/shared/ui';
import { getErrorMessage } from '@hasura/shared/utils';

const TriggerContainer: React.FC = () => {
  const params = useParams();
  const {
    data: meta,
    isLoading: metadataLoading,
    error,
  } = useMetadata(undefined, {
    enabled: Boolean(params.triggerName),
  });

  const currentTrigger = params.triggerName
    ? MetadataSelectors.selectEventTriggerByName(params.triggerName)(meta)
    : undefined;

  if (!currentTrigger) {
    if (metadataLoading) {
      return <SkeletonList count={5} />;
    }

    return (
      <IndicatorCard status="negative" showIcon>
        {error ? getErrorMessage(error) : 'Could not find any event trigger'}
      </IndicatorCard>
    );
  }

  return (
    <EventTriggerDetailContext.Provider
      value={{
        eventTrigger: currentTrigger.eventTrigger,
        currentSource: currentTrigger.source,
        currentTable: currentTrigger.table,
      }}
    >
      <Outlet />
    </EventTriggerDetailContext.Provider>
  );
};

export default TriggerContainer;
