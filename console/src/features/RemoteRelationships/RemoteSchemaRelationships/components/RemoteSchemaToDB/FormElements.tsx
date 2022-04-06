import { useRemoteSchema } from '@/features/MetadataAPI';
// eslint-disable-next-line no-restricted-imports
import { useTableColumns } from '@/features/SqlQueries/hooks/useTableColumns';
import { InputField, Select } from '@/new-components/Form';
import { MapSelector } from '@/new-components/MapSelector';
import React, { useEffect, useState } from 'react';
import { useFormContext } from 'react-hook-form';
import { FaLink } from 'react-icons/fa';
import { RemoteDatabaseWidget } from '../RemoteDatabaseWidget';
import { RsSourceTypeSelector } from '../RsSourceTypeSelector';
import { Schema } from './schema';
import { getTypesFromIntrospection } from './utils';

export const FormElements = ({
  sourceRemoteSchema,
}: {
  sourceRemoteSchema: string;
}) => {
  const { watch } = useFormContext<Schema>();

  const database = watch('database');
  const schema = watch('schema');
  const table = watch('table');
  const RSTypeName = watch('typeName');
  const { fetchSchema, data, isLoading } = useRemoteSchema();

  const { data: columnData } = useTableColumns(database, {
    name: table,
    schema,
  });

  const columns: string[] = columnData
    ? columnData.slice(1).map((x: string[]) => x[3])
    : [];

  React.useEffect(() => {
    if (sourceRemoteSchema) {
      fetchSchema(sourceRemoteSchema);
    }
  }, [fetchSchema, sourceRemoteSchema]);

  const [typeMap, setTypeMap] = useState<{ field: string; column: string }[]>(
    []
  );

  useEffect(() => {
    setTypeMap([]);
  }, [RSTypeName]);

  if (!data)
    return (
      <div className="grid border border-gray-300 rounded shadow-sm p-4">
        Data is not ready
      </div>
    );

  if (isLoading)
    return (
      <div className="grid border border-gray-300 rounded shadow-sm p-4">
        Loading...
      </div>
    );

  const remoteSchemaTypes = getTypesFromIntrospection(data);

  return (
    <>
      <hr className="mb-md border-gray-300" />

      {/* relationship meta */}
      <div className="mb-md">
        <div className="grid gap-sm grid-cols-1 sm:grid-cols-2">
          <div className="grid gap-sm grid-cols-1 sm:grid-cols-2">
            <div className="bg-white shadow-sm rounded p-md border border-gray-300">
              <p className="flex items-center font-semibold text-muted">
                <label className="cursor-pointer ml-sm font-semibold">
                  Remote Schema Relationship
                </label>
              </p>
              <p className="text-muted pl-6">
                Relationship from this remote schema to another
                database&nbsp;schema.
              </p>
            </div>
          </div>
        </div>
      </div>
      <div className="w-full sm:w-6/12 mb-md">
        <div className="mb-md">
          <InputField
            name="relationshipName"
            label="Name"
            placeholder="Relationship name"
            dataTest="rs-to-db-rel-name"
          />
        </div>

        <div className="mb-md">
          <Select
            name="relationshipType"
            label="Type"
            dataTest="select-rel-type"
            placeholder="Select a relationship type..."
            options={[
              {
                label: 'Array Relationship',
                value: 'array',
              },
              {
                label: 'Object Relationship',
                value: 'object',
              },
            ]}
          />
        </div>
      </div>

      <div className="grid grid-cols-12">
        <div className="col-span-5">
          <RsSourceTypeSelector
            types={remoteSchemaTypes.map(t => t.typeName)}
            sourceTypeKey="typeName"
          />
        </div>

        {/* horizontal connector line */}
        <div className="col-span-2 flex relative items-center justify-center w-full py-md">
          <div
            className="flex z-10 items-center justify-center border border-gray-300 bg-white"
            style={{
              height: '32px',
              width: '32px',
              borderRadius: '100px',
            }}
          >
            <FaLink />
          </div>
          <div className="absolute w-full border-b border-gray-300" />
        </div>

        <div className="col-span-5">
          <RemoteDatabaseWidget />
        </div>
      </div>

      {/* vertical connector line */}
      <div className="flex items-center w-full px-8">
        <div className="relative flex items-center justify-center h-20">
          <div className="absolute border border-l border-gray-300 h-20" />
          <div className="absolute border border-gray-300 flex items-center justify-center rounded-full h-10 w-10 bg-white">
            <FaLink />
          </div>
        </div>
        <p className="m-0 px-8 font-semibold">Type Mapped To</p>
      </div>

      <MapSelector
        types={
          remoteSchemaTypes.find(x => x.typeName === RSTypeName)?.fields ?? []
        }
        columns={columns}
        typeMappings={typeMap}
        placeholder=""
        name="mapping"
        onChange={e => {
          setTypeMap([...e]);
        }}
      />
    </>
  );
};
