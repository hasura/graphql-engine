import { useDatabaseHierarchy } from '@/features/Data';
import { getTableName } from '@/features/DataSource';
import { Table } from '@/features/MetadataAPI';
import { InputField, Select } from '@/new-components/Form';
import React from 'react';
import { useWatch } from 'react-hook-form';
import { FaDatabase, FaTable } from 'react-icons/fa';
import Skeleton from 'react-loading-skeleton';
import { useGetTargetOptions } from '../hooks/useGetTargetOptions';

export const TargetTable = ({ disabled }: { disabled: boolean }) => {
  const table = useWatch<Record<string, Table>>({ name: 'source_table' });
  const dataSourceName = useWatch<Record<string, string>>({
    name: 'target_name',
  });
  const {
    data: databaseHierarchy,
    isLoading: isDatabaseHierarchyLoading,
    error: databaseHierarchyError,
  } = useDatabaseHierarchy(dataSourceName);
  const {
    data: options,
    isLoading: areTargetOptionsLoading,
    error: targetOptionError,
  } = useGetTargetOptions(dataSourceName, table);

  if (databaseHierarchyError)
    return (
      <div className="h-full">
        {JSON.stringify(databaseHierarchyError.response?.data)}
      </div>
    );

  if (targetOptionError)
    return (
      <div className="h-full">
        {JSON.stringify(targetOptionError.response?.data)}
      </div>
    );

  if (areTargetOptionsLoading || isDatabaseHierarchyLoading)
    return (
      <div className="h-full">
        <div data-testid="target_table_loading">
          <Skeleton count={3} height={40} className="my-3" />
        </div>
      </div>
    );

  return (
    <div className="h-full">
      <InputField
        name="target_name"
        label="Reference Database"
        labelIcon={<FaDatabase />}
        disabled
      />
      <Select
        name="target_table"
        label="Reference Table"
        labelIcon={<FaTable />}
        options={(options ?? []).map(t => ({
          value: JSON.stringify(t),
          label: getTableName(t, databaseHierarchy ?? []),
        }))}
        disabled={disabled}
      />
    </div>
  );
};
