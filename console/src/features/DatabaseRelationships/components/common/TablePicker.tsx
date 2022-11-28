import { Table } from '@/features/hasura-metadata-types';
import clsx from 'clsx';
import React from 'react';
import { useFormContext, useWatch } from 'react-hook-form';
import { FaDatabase, FaTable } from 'react-icons/fa';
import Skeleton from 'react-loading-skeleton';
import { useMetadata, MetadataSelector } from '@/features/hasura-metadata-api';
import { getTableDisplayName } from '../../utils/helpers';

type InputOptions = {
  label?: string;
  placeholder?: string;
  disabled?: boolean;
};

interface TablePickerProps {
  name: string;
  options?: {
    dataSource?: InputOptions;
    table?: InputOptions;
  };
}

// TODO: add display for form errors
export const TablePicker = (props: TablePickerProps) => {
  const { name, options } = props;

  const { setValue } = useFormContext();

  const currentDataSourceName = useWatch<Record<string, string | undefined>>({
    name: `${name}.dataSourceName`,
  });

  const currentTable = useWatch<Record<string, Table>>({
    name: `${name}.table`,
  });

  const { data: metadataTables } = useMetadata(
    MetadataSelector.getMetadataTables(currentDataSourceName ?? '')
  );

  const { data: metadataSources } = useMetadata(
    MetadataSelector.getMetadataSources()
  );

  if (!metadataSources) return <Skeleton count={5} height={20} />;

  return (
    <div className="h-full">
      <div className="my-2">
        <div className="flex items-center gap-2 text-gray-600 mb-xs">
          <FaDatabase />
          <label className="font-semibold">
            {options?.dataSource?.label ?? 'Select a source'}
          </label>
        </div>
        <select
          className={clsx(
            'block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400',
            options?.dataSource?.disabled
              ? 'cursor-not-allowed bg-gray-200 border-gray-200 hover:border-gray-200'
              : 'hover:border-gray-400'
          )}
          disabled={!metadataSources || options?.dataSource?.disabled}
          value={currentDataSourceName}
          data-testid={`${name}.table`}
          onChange={e => {
            setValue(name, { dataSourceName: e.target.value });
          }}
        >
          <option value="" data-default-selected hidden>
            {options?.dataSource?.placeholder ?? 'Select a source...'}
          </option>

          {metadataSources.map((s, i) => (
            <option value={s.name} key={i}>
              {s.name}
            </option>
          ))}
        </select>
      </div>

      <div className="my-2">
        <div className="flex items-center gap-2 text-gray-600 mb-xs">
          <FaTable />
          <label className="font-semibold">
            {options?.table?.label ?? 'Select a table'}
          </label>
        </div>
        <select
          className={clsx(
            'block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400',
            options?.table?.disabled
              ? 'cursor-not-allowed bg-gray-200 border-gray-200 hover:border-gray-200'
              : 'hover:border-gray-400'
          )}
          disabled={!metadataTables || options?.table?.disabled}
          value={JSON.stringify(currentTable)}
          data-testid={`${name}.table`}
          onChange={e => {
            setValue(`${name}.table`, JSON.parse(e.target.value));
          }}
        >
          <option value="" data-default-selected hidden>
            {options?.table?.placeholder ?? 'Select a table...'}
          </option>

          {(metadataTables ?? []).map((t, i) => (
            <option value={JSON.stringify(t.table)} key={i}>
              {getTableDisplayName(t.table)}
            </option>
          ))}
        </select>
      </div>
    </div>
  );
};
