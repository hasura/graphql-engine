import React, { useEffect } from 'react';
import { useFormContext } from 'react-hook-form';
import { FaDatabase, FaFolder, FaTable } from 'react-icons/fa';
import { Select } from '@/new-components/Form';
import { useSources } from '@/features/MetadataAPI';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import { MetadataDataSource } from '@/metadata/types';

const useLoadData = (sources?: MetadataDataSource[]) => {
  const { watch, setValue } = useFormContext();

  const sourceName = watch('database');
  const schemaName = watch('schema');

  const source = React.useMemo(
    () => sources?.find(({ name }) => name === sourceName),
    [sourceName, sources]
  );
  const driver = source?.kind;

  useEffect(() => {
    const isSchemaPresentInSource = source?.tables.find(
      (x: any) =>
        x.table[driver === 'bigquery' ? 'dataset' : 'schema'] === schemaName
    );

    if (!isSchemaPresentInSource) {
      setValue('schema', '');
      setValue('table', '');
    }

    setValue('driver', driver);
  }, [sourceName, driver, setValue]);

  const sourceOptions = React.useMemo(() => {
    return sources?.map(({ name }) => ({ value: name, label: name })) || [];
  }, [sources]);

  const schemaOptions = React.useMemo(() => {
    return (
      Array.from(
        new Set(
          source?.tables.map(
            tableObj => tableObj.table.schema ?? (tableObj as any).table.dataset
          )
        )
      ).map(value => ({ value, label: value })) || []
    );
  }, [source]);

  const tableOptions = React.useMemo(() => {
    return (
      source?.tables
        ?.filter(
          tableObj =>
            (tableObj as any)?.table?.[
              driver === 'bigquery' ? 'dataset' : 'schema'
            ] === schemaName
        )
        .map(tableObj => ({
          value: tableObj?.table?.name,
          label: tableObj?.table?.name,
        })) || []
    );
  }, [source, schemaName, driver]);

  return { sourceOptions, schemaOptions, tableOptions, driver };
};

export const RemoteDatabaseWidget = () => {
  const { data: sources, isError: sourcesError } = useSources();
  const { sourceOptions, schemaOptions, tableOptions, driver } = useLoadData(
    sources
  );

  if (sourcesError) {
    return (
      <IndicatorCard status="negative">Error loading database</IndicatorCard>
    );
  }

  return (
    <div className="bg-gray-50 col-span-5 rounded p-md border border-gray-300 border-l-4 border-l-indigo-600">
      <div className="mb-sm">
        <Select
          label="Reference Database"
          name="database"
          placeholder="Select a database"
          options={sourceOptions}
          dataTest="select-ref-db"
          labelIcon={<FaDatabase />}
        />
      </div>
      <div className="mb-sm">
        <Select
          label={`Reference ${driver === 'bigquery' ? 'Dataset' : 'Schema'}`}
          name="schema"
          placeholder={`Select a ${
            driver === 'bigquery' ? 'dataset' : 'schema'
          }`}
          options={schemaOptions}
          dataTest="select-ref-schema"
          labelIcon={<FaFolder />}
        />
      </div>
      <div className="mb-sm">
        <Select
          label="Reference Table"
          name="table"
          placeholder="Select a table"
          options={tableOptions}
          dataTest="select-ref-table"
          labelIcon={<FaTable />}
        />
      </div>
    </div>
  );
};
