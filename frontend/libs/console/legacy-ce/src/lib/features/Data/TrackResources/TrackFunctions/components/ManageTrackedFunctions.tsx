import React from 'react';
import { TrackableResourceTabs } from '../../components/TrackableResourceTabs';
import { UntrackedFunctions } from './UntrackedFunctions';
import { useUntrackedFunctions } from '../hooks/useUntrackedFunctions';
import {
  MetadataSelectors,
  useMetadata,
} from '../../../../hasura-metadata-api';
import { adaptFunctionName } from '../utils';
import { TrackedFunctions } from './TrackedFunctions';

type TabState = 'tracked' | 'untracked';

export const ManageTrackedFunctions = ({
  dataSourceName,
}: {
  dataSourceName: string;
}) => {
  const [tab, setTab] = React.useState<TabState>('untracked');

  const { data: untrackedFunctions = [], isSuccess } =
    useUntrackedFunctions(dataSourceName);

  const { data: trackedFunctions = [] } = useMetadata(m =>
    (MetadataSelectors.findSource(dataSourceName)(m)?.functions ?? []).map(
      fn => ({
        qualifiedFunction: fn.function,
        name: adaptFunctionName(fn.function).join(' / '),
      })
    )
  );

  return (
    <TrackableResourceTabs
      introText={
        'Tracking functions adds them to your GraphQL API. All objects will be admin-only until permissions have been set.'
      }
      value={tab}
      onValueChange={value => {
        setTab(value);
      }}
      isLoading={!isSuccess}
      items={{
        untracked: {
          amount: untrackedFunctions.length,
          content: <UntrackedFunctions dataSourceName={dataSourceName} />,
        },
        tracked: {
          amount: trackedFunctions.length,
          content: <TrackedFunctions dataSourceName={dataSourceName} />,
        },
      }}
    />
  );
};
