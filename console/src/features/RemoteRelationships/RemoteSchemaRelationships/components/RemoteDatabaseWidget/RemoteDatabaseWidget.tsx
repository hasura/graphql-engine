import React from 'react';
import { useFormContext } from 'react-hook-form';
import { FaDatabase, FaFolder, FaTable } from 'react-icons/fa';
import { Select } from '@/new-components/Form';
import { useSources } from '@/features/MetadataAPI';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import { MetadataDataSource } from '@/metadata/types';

const useLoadData = (sources?: MetadataDataSource[]) => {
  const { watch } = useFormContext();

  const sourceName = watch('database');
  const schemaName = watch('schema');

  const source = React.useMemo(
    () => sources?.find(({ name }) => name === sourceName),
    [sourceName, sources]
  );

  const sourceOptions = React.useMemo(() => {
    return (
      sources
        ?.filter(s => !['bigquery', 'mssql'].includes(s.kind))
        .map(({ name }) => ({ value: name, label: name })) || []
    );
  }, [sources]);

  const schemaOptions = React.useMemo(() => {
    return (
      Array.from(
        new Set(source?.tables.map(tableObj => tableObj.table.schema))
      ).map(value => ({ value, label: value })) || []
    );
  }, [source]);

  const tableOptions = React.useMemo(() => {
    return (
      source?.tables
        ?.filter(tableObj => tableObj?.table?.schema === schemaName)
        .map(tableObj => ({
          value: tableObj?.table?.name,
          label: tableObj?.table?.name,
        })) || []
    );
  }, [source, schemaName]);

  return { sourceOptions, schemaOptions, tableOptions };
};

export const RemoteDatabaseWidget = () => {
  const { data: sources, isError: sourcesError } = useSources();
  const { sourceOptions, schemaOptions, tableOptions } = useLoadData(sources);

  if (sourcesError) {
    return (
      <IndicatorCard status="negative">Error loading database</IndicatorCard>
    );
  }

  return (
    <div className="rounded bg-gray-50 border border-gray-300 p-md">
      <div className="grid grid-cols-2 gap-3 mb-md">
        <Select
          label="Reference Database"
          name="database"
          placeholder="Select a database"
          options={sourceOptions}
          labelIcon={<FaDatabase />}
        />
      </div>
      <div className="grid grid-cols-2 gap-3 mb-md">
        <div>
          <Select
            label="Reference Schema"
            name="schema"
            placeholder="Select a schema"
            options={schemaOptions}
            labelIcon={<FaFolder />}
          />
        </div>
        <div>
          <Select
            label="Reference Table"
            name="table"
            placeholder="Select a table"
            options={tableOptions}
            labelIcon={<FaTable />}
          />
        </div>
      </div>
    </div>
  );
};
