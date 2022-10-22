import React, { useEffect, useState } from 'react';
import { useFormContext } from 'react-hook-form';
import { useRemoteSchema } from '@/features/MetadataAPI';
// eslint-disable-next-line no-restricted-imports
import { useTableColumns } from '@/features/SqlQueries/hooks/useTableColumns';
import { InputField, Select } from '@/new-components/Form';
import { MapSelector } from '@/new-components/MapSelector';
import {
  LinkBlockHorizontal,
  LinkBlockVertical,
} from '@/new-components/LinkBlock';
import { RemoteDatabaseWidget } from '../RemoteDatabaseWidget';
import { RsSourceTypeSelector } from '../RsSourceTypeSelector';
import { Schema } from './schema';
import { getTypesFromIntrospection } from '../../utils';

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
      <div className="w-full sm:w-6/12 my-md">
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

        <LinkBlockHorizontal />

        <div className="col-span-5">
          <RemoteDatabaseWidget />
        </div>
      </div>

      {/* vertical connector line */}
      <LinkBlockVertical title="Type Mapped To" />
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
