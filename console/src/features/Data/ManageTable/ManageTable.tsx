import { Table } from '@/features/MetadataAPI';
import { Badge } from '@/new-components/Badge';
import { DropdownMenu } from '@/new-components/DropdownMenu';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import { Tabs } from '@/new-components/Tabs';
import React, { useState } from 'react';
import { FaAngleRight, FaChevronDown, FaDatabase } from 'react-icons/fa';
import { useDatabaseHierarchy } from '../hooks';
import { getTableName } from '../TrackTables/hooks/useTables';

interface ManageTableProps {
  dataSourceName: string;
  table: Table;
}

const FeatureNotImplemented = () => {
  return (
    <div className="my-4">
      <IndicatorCard headline="Feature is currently unavailable">
        Feature not implemented
      </IndicatorCard>
    </div>
  );
};

const availableTabs = [
  {
    value: 'Browse',
    label: 'Browse',
    content: <FeatureNotImplemented />,
  },
  {
    value: 'Modify',
    label: 'Modify',
    content: <FeatureNotImplemented />,
  },
  {
    value: 'Relationships',
    label: 'Relationships',
    content: <FeatureNotImplemented />,
  },
  {
    value: 'Permissions',
    label: 'Permissions',
    content: <FeatureNotImplemented />,
  },
];

export const ManageTable = (props: ManageTableProps) => {
  const { table, dataSourceName } = props;

  const { data: databaseHierarchy, isLoading } =
    useDatabaseHierarchy(dataSourceName);

  const [tab, setTab] = useState('Browse');

  if (isLoading) return <IndicatorCard status="info">Loading...</IndicatorCard>;

  if (!databaseHierarchy)
    return (
      <IndicatorCard status="negative">
        Could not fetch the database hierarchy for the table.
      </IndicatorCard>
    );

  const tableName = getTableName(table, databaseHierarchy);

  return (
    <div className="w-full overflow-y-auto bg-gray-50">
      <div className="px-md pt-md mb-xs">
        <div className="flex items-center space-x-xs mb-1">
          <div className="cursor-pointer flex items-center text-muted hover:text-gray-900">
            <FaDatabase className="mr-1.5" />
            <span className="text-sm">{dataSourceName}</span>
          </div>
          <FaAngleRight className="text-muted" />
          <div className="cursor-pointer flex items-center text-muted hover:text-gray-900">
            <FaDatabase className="mr-1.5" />
            <span className="text-sm">{tableName}</span>
          </div>
          <FaAngleRight className="text-muted" />
          <div className="cursor-pointer flex items-center">
            <span className="text-sm font-semibold text-yellow-500">
              Manage
            </span>
          </div>
        </div>
        <br />
        <div className="flex items-center gap-3">
          <div className="group relative">
            <div>
              <DropdownMenu
                items={[
                  [
                    // TODO: To be implemented after metadata util functions have been added to the metadata library
                    <span className="py-xs text-red-600" onClick={() => {}}>
                      Untrack {tableName}
                    </span>,
                  ],
                ]}
              >
                <div className="flex gap-0.5 items-center">
                  <h1 className="inline-flex items-center text-xl font-semibold mb-1">
                    {tableName}
                  </h1>
                  <FaChevronDown className="text-gray-400 text-sm transition-transform group-radix-state-open:rotate-180" />
                </div>
              </DropdownMenu>
            </div>
          </div>
          <div>
            <Badge color="green">Tracked</Badge>
          </div>
        </div>

        <Tabs value={tab} onValueChange={setTab} items={availableTabs} />
      </div>
    </div>
  );
};
