import React from 'react';

import { UntrackedFunctions } from './UntrackedFunctions';
import { useUntrackedFunctions } from '../hooks/useUntrackedFunctions';
import {
  MetadataSelectors,
  useMetadata,
} from '../../../../hasura-metadata-api';
import { adaptFunctionName } from '../utils';
import { TrackedFunctions } from './TrackedFunctions';
import { TrackableResourceTabs } from '../../../ManageDatabase/components';
import { PostgresTable } from '../../../../DataSource';

type TabState = 'tracked' | 'untracked';

export const ManageTrackedFunctions = ({
  dataSourceName,
  schema,
}: {
  dataSourceName: string;
  schema?: string;
}) => {
  const [tab, setTab] = React.useState<TabState>('untracked');

  const {
    data: untrackedFunctions = [],
    isLoading: isUntrackedFunctionsLoading,
  } = useUntrackedFunctions(dataSourceName, schema);

  const { data: trackedFunctions = [], isLoading: isTrackedFunctionsLoading } =
    useMetadata(m => {
      const result = (
        MetadataSelectors.findSource(dataSourceName)(m)?.functions ?? []
      ).map(fn => ({
        qualifiedFunction: fn.function,
        name: adaptFunctionName(fn.function).join(' / '),
      }));

      if (!schema) return result;

      return result.filter(
        fn => (fn.qualifiedFunction as PostgresTable).schema === schema
      );
    });

  return (
    <TrackableResourceTabs
      introText={
        'Tracking functions adds them to your GraphQL API. All objects will be admin-only until permissions have been set.'
      }
      value={tab}
      onValueChange={value => {
        setTab(value);
      }}
      isLoading={isUntrackedFunctionsLoading || isTrackedFunctionsLoading}
      items={{
        untracked: {
          amount: untrackedFunctions.length,
          content: (
            <UntrackedFunctions
              dataSourceName={dataSourceName}
              isLoading={isUntrackedFunctionsLoading}
              untrackedFunctions={untrackedFunctions}
            />
          ),
        },
        tracked: {
          amount: trackedFunctions.length,
          content: (
            <TrackedFunctions
              dataSourceName={dataSourceName}
              isLoading={isTrackedFunctionsLoading}
              trackedFunctions={trackedFunctions}
            />
          ),
        },
      }}
    />
  );
};
