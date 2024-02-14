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
import { ReactQueryStatusUI } from '../../../components';
import { multipleQueryUtils } from '../../../components/ReactQueryWrappers/utils';

type TabState = 'tracked' | 'untracked';

export const ManageTrackedFunctions = ({
  dataSourceName,
  schema,
}: {
  dataSourceName: string;
  schema?: string;
}) => {
  const [tab, setTab] = React.useState<TabState>('untracked');

  const untrackedFunctionsResult = useUntrackedFunctions(
    dataSourceName,
    schema
  );

  const metadataResult = useMetadata(m => {
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

  if (!untrackedFunctionsResult.isSuccess || !metadataResult.isSuccess) {
    const results = [metadataResult, untrackedFunctionsResult];
    return (
      <ReactQueryStatusUI
        status={multipleQueryUtils.status(results)}
        error={multipleQueryUtils.firstError(results)}
      />
    );
  }

  const { data: untrackedFunctions } = untrackedFunctionsResult;
  const { data: trackedFunctions } = metadataResult;

  return (
    <TrackableResourceTabs
      introText={
        'Tracking functions adds them to your GraphQL API. All objects will be admin-only until permissions have been set.'
      }
      value={tab}
      onValueChange={value => {
        setTab(value);
      }}
      items={{
        untracked: {
          amount: untrackedFunctions.length,
          content: (
            <UntrackedFunctions
              dataSourceName={dataSourceName}
              untrackedFunctions={untrackedFunctions}
            />
          ),
        },
        tracked: {
          amount: trackedFunctions.length,
          content: (
            <TrackedFunctions
              dataSourceName={dataSourceName}
              trackedFunctions={trackedFunctions}
            />
          ),
        },
      }}
    />
  );
};
