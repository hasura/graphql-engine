import { useDatabaseHierarchy } from '@/features/Data';
import { getTableName } from '@/features/DataSource';
import { Table } from '@/features/MetadataAPI';
import { InputField, Select } from '@/new-components/Form';
import React from 'react';
import { useWatch } from 'react-hook-form';
import { FaDatabase, FaTable } from 'react-icons/fa';
import Skeleton from 'react-loading-skeleton';

export const SourceTable = () => {
  const table = useWatch<Record<string, Table>>({ name: 'source_table' });

  const dataSourceName = useWatch<Record<string, string>>({
    name: 'source_name',
  });

  const {
    data: databaseHierarchy,
    isLoading: isDatabaseHierarchyLoading,
    error: databaseHierarchyError,
  } = useDatabaseHierarchy(dataSourceName);

  if (databaseHierarchyError)
    return (
      <div className="h-full">
        {JSON.stringify(databaseHierarchyError.response?.data)}
      </div>
    );

  if (isDatabaseHierarchyLoading)
    return (
      <div className="h-full">
        <div data-testid="target_table_loading">
          <Skeleton count={3} height={40} className="my-3" />
        </div>
      </div>
    );

  return (
    <div>
      <InputField
        name="source_name"
        label="Source Database"
        labelIcon={<FaDatabase />}
        disabled
      />
      <Select
        name="source_table"
        options={[table].map(t => ({
          value: t,
          label: getTableName(t, databaseHierarchy ?? []),
        }))}
        label="Source Table"
        labelIcon={<FaTable />}
        disabled
      />
    </div>
  );
};
