import { RiInformationFill } from 'react-icons/ri';
import { Collapsible } from '../../../../new-components/Collapsible';
import * as Tabs from '@radix-ui/react-tabs';
import { Tooltip } from '../../../../new-components/Tooltip';
import React, { useMemo } from 'react';
import { useTables } from '../hooks/useTables';
import Skeleton from 'react-loading-skeleton';
import { TableList } from './TableList';

const classNames = {
  selected:
    'border-yellow-500 text-yellow-500 whitespace-nowrap p-xs border-b-2 font-semibold -mb-0.5',
  unselected:
    'border-transparent text-muted whitespace-nowrap p-xs border-b-2 font-semibold -mb-0.5 hover:border-gray-300 hover:text-gray-900',
};

export const ManageTrackedTables = ({
  dataSourceName,
}: {
  dataSourceName: string;
}) => {
  const [tab, setTab] = React.useState<'tracked' | 'untracked'>('untracked');

  const { data, isLoading } = useTables({
    dataSourceName,
  });

  const trackedTables = useMemo(
    () => (data ?? []).filter(table => table.is_tracked),
    [data]
  );

  const untrackedTables = useMemo(
    () => (data ?? []).filter(table => !table.is_tracked),
    [data]
  );

  if (isLoading)
    return (
      <div className="px-md">
        <Skeleton count={8} height={25} className="mb-2" />
      </div>
    );

  if (!data) return <div className="px-md">Something went wrong</div>;

  return (
    <Collapsible
      triggerChildren={
        <div>
          <div className="flex mb-1 items-center">
            <div className="font-semibold inline-flex items-center text-lg">
              Untracked Tables/Views
            </div>
            <Tooltip
              tooltipContentChildren="Expose the tables available in your database via the GraphQL API"
              side="right"
            >
              <RiInformationFill />
            </Tooltip>
          </div>
        </div>
      }
      defaultOpen
    >
      <Tabs.Root
        defaultValue="untracked"
        data-testid="track-tables"
        className="space-y-4"
        onValueChange={value =>
          setTab(value === 'tracked' ? 'tracked' : 'untracked')
        }
      >
        <p className="text-muted">
          Tracking tables adds them to your GraphQL API. All objects will be
          admin-only until permissions have been set.
        </p>
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
          <TableList
            mode={'track'}
            dataSourceName={dataSourceName}
            tables={trackedTables}
          />
        </Tabs.Content>
        <Tabs.Content value="untracked" className="px-md">
          <TableList
            mode={'untrack'}
            dataSourceName={dataSourceName}
            tables={untrackedTables}
          />
        </Tabs.Content>
      </Tabs.Root>
    </Collapsible>
  );
};
