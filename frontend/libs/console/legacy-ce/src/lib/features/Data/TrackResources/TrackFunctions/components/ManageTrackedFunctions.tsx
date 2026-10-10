import React from 'react';
import { UntrackedFunctions } from './UntrackedFunctions';
import { TrackedFunctions } from './TrackedFunctions';
import { TrackableResourceTabs } from '../../../ManageDatabase/components';
import {
  IntrospectedFunction,
  useTrackedAndUntrackedFunctions,
} from '@hasura/metadata/data-source';
import { IndicatorCard, SkeletonList } from '@hasura/shared/ui';
import { getErrorMessage } from '@hasura/shared/utils';
import { Source } from '@hasura/shared/types';

type TabState = 'tracked' | 'untracked';

export const ManageTrackedFunctions = ({
  dataSourceName,
  schema,
}: {
  dataSourceName: string;
  schema?: string;
}) => {
  const { trackedFunctions, untrackedFunctions, isLoading, error, source } =
    useTrackedAndUntrackedFunctions({ dataSourceName, schema });

  if (isLoading) {
    return <SkeletonList count={5} />;
  }

  if (error || !source) {
    return (
      <IndicatorCard status="negative" headline="Failed to fetch functions">
        {getErrorMessage(error)}
      </IndicatorCard>
    );
  }

  return (
    <ManageTrackedFunctionsContent
      source={source}
      trackedFunctions={trackedFunctions ?? []}
      untrackedFunctions={untrackedFunctions ?? []}
    />
  );
};

export const ManageTrackedFunctionsContent = ({
  source,
  trackedFunctions,
  untrackedFunctions,
}: {
  source: Source;
  trackedFunctions: IntrospectedFunction[];
  untrackedFunctions: IntrospectedFunction[];
}) => {
  const [tab, setTab] = React.useState<TabState>(
    untrackedFunctions.length ? 'untracked' : 'tracked',
  );

  return (
    <TrackableResourceTabs
      introText={
        'Tracking functions adds them to your GraphQL API. All objects will be admin-only until permissions have been set.'
      }
      value={tab}
      onValueChange={(value) => {
        setTab(value);
      }}
      items={{
        untracked: {
          amount: untrackedFunctions?.length ?? 0,
          content: (
            <UntrackedFunctions
              dataSourceName={source.name}
              untrackedFunctions={untrackedFunctions ?? []}
            />
          ),
        },
        tracked: {
          amount: trackedFunctions.length,
          content: (
            <TrackedFunctions
              source={source}
              trackedFunctions={trackedFunctions ?? []}
            />
          ),
        },
      }}
    />
  );
};
