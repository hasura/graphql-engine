import React, { useState, useEffect } from 'react';
import { useFormContext } from 'react-hook-form';
import { FaPlug } from 'react-icons/fa';

import { IndicatorCard } from '@/new-components/IndicatorCard';
import { useRemoteSchema } from '@/features/MetadataAPI';
import { RemoteRelationship } from '@/metadata/types';

import {
  RemoteSchemaTree,
  buildServerRemoteFieldObject,
  parseServerRelationship,
} from '../RemoteSchemaTree';

import {
  HasuraRsFields,
  AllowedRootFields,
  RelationshipFields,
} from '../../types';

export interface RemoteSchemaWidgetProps {
  schemaName: string;
  /**
   * Columns array from the current table.
   */
  fields: HasuraRsFields;
  /**
   * Remote relationship object from server, for already present permissions.
   * This will be parsed and tree will be populated accordingly
   */
  serverRelationship?: RemoteRelationship;
  rootFields?: AllowedRootFields;
}

const resultSet = 'resultSet';

export const RemoteSchemaWidget = ({
  schemaName,
  fields,
  serverRelationship,
  rootFields = ['query'],
}: RemoteSchemaWidgetProps) => {
  const { fetchSchema, data, isLoading, isError } = useRemoteSchema();

  const { setValue, watch } = useFormContext();
  const resultSetValue = watch(resultSet);

  const [relationshipFields, setRelationshipFields] = useState<
    RelationshipFields[]
  >([]);

  useEffect(() => {
    if (serverRelationship) {
      setRelationshipFields(parseServerRelationship(serverRelationship));
    }
  }, [serverRelationship, setValue]);

  useEffect(() => {
    const value = buildServerRemoteFieldObject(relationshipFields);
    const jsonValue = JSON.stringify(value ?? '');
    setValue('resultSet', jsonValue);
  }, [relationshipFields, setValue]);

  useEffect(() => {
    if (schemaName) {
      fetchSchema(schemaName);
    }
  }, [fetchSchema, schemaName]);

  return (
    <div className="grid gap-4 border border-gray-300 rounded shadow-sm p-4 bg-gray-50">
      <div className="grid gap-4 w-full">
        <label className="block ">
          <span className="text-gray-600 mb-xs font-semibold">Result Set</span>
          <input
            type="text"
            className="mt-xs block h-input w-full shadow-sm rounded cursor-not-allowed bg-gray-100 border border-gray-300"
            value={resultSetValue}
            disabled
          />
        </label>
      </div>
      <hr />

      {isLoading && <div>Loading...</div>}

      <div className="py-2 px-1 rounded-md border border-gray-300 bg-white">
        <p className="flex gap-2 items-center font-semibold p-2 m-0">
          <FaPlug />
          {schemaName}
        </p>

        {data && (
          <RemoteSchemaTree
            schema={data}
            relationshipFields={relationshipFields}
            setRelationshipFields={setRelationshipFields}
            fields={fields}
            rootFields={rootFields}
          />
        )}
      </div>

      {isError && (
        <IndicatorCard status="negative">
          Error loading remote schema
        </IndicatorCard>
      )}
    </div>
  );
};
