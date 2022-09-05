import React from 'react';
import * as Tabs from '@radix-ui/react-tabs';
import { useTables } from './hooks/useTables';

import { TrackedTables } from './components/TrackedTables';
import { UntrackedTables } from './components/UntrackedTables';

const classNames = {
  selected:
    'border-yellow-500 text-yellow-500 whitespace-nowrap p-xs border-b-2 font-semibold -mb-0.5',
  unselected:
    'border-transparent text-muted whitespace-nowrap p-xs border-b-2 font-semibold -mb-0.5 hover:border-gray-300 hover:text-gray-900',
};

interface Props {
  dataSourceName: string;
}

export const TrackTables = ({ dataSourceName }: Props) => {
  const [tab, setTab] = React.useState<'tracked' | 'untracked'>('untracked');

  const { data, isLoading } = useTables({
    dataSourceName,
  });

  if (isLoading) return <>Loading...</>;

  if (!data) return <>Something went wrong</>;

  const trackedTables = data.filter(({ is_tracked }) => is_tracked);
  const untrackedTables = data.filter(({ is_tracked }) => !is_tracked);

  return (
    <Tabs.Root
      defaultValue="untracked"
      className="space-y-4"
      onValueChange={value =>
        setTab(value === 'tracked' ? 'tracked' : 'untracked')
      }
    >
      <Tabs.List
        className="border-b border-gray-300 px-4 flex space-x-4"
        aria-label="Tabs"
      >
        <Tabs.Trigger
          value="untracked"
          className={
            tab === 'untracked' ? classNames.selected : classNames.unselected
          }
        >
          Untracked
          <span className="bg-gray-300 ml-1 px-1.5 py-0.5 rounded text-xs">
            {untrackedTables.length}
          </span>
        </Tabs.Trigger>
        <Tabs.Trigger
          value="tracked"
          className={
            tab === 'tracked' ? classNames.selected : classNames.unselected
          }
        >
          Tracked
          <span className="bg-gray-300 ml-1 px-1.5 py-0.5 rounded text-xs">
            {trackedTables.length}
          </span>
        </Tabs.Trigger>
      </Tabs.List>

      <Tabs.Content value="tracked" className="px-md">
        <TrackedTables dataSourceName={dataSourceName} tables={trackedTables} />
      </Tabs.Content>
      <Tabs.Content value="untracked" className="px-md">
        <UntrackedTables
          dataSourceName={dataSourceName}
          tables={untrackedTables}
        />
      </Tabs.Content>
    </Tabs.Root>
  );
};
