import React, { useState, useEffect } from 'react';
import { useFormContext } from 'react-hook-form';

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
  RsToRsSchema,
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
  rootFields = ['query'],
}: RemoteSchemaWidgetProps) => {
  const { fetchSchema, data, isLoading, isError } = useRemoteSchema();

  const { setValue, watch } = useFormContext<RsToRsSchema>();
  const resultSetValue = watch(resultSet);
  const relationship = watch('relationship');

  const [relationshipFields, setRelationshipFields] = useState<
    RelationshipFields[]
  >([]);

  useEffect(() => {
    if (relationship) {
      setRelationshipFields(parseServerRelationship(relationship));
    }
  }, [relationship, setValue]);

  useEffect(() => {
    const value = buildServerRemoteFieldObject(relationshipFields);
    setValue('resultSet', value);
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
          <span className="text-gray-600 mb-xs font-semibold">Mapping</span>
          <input
            type="text"
            className="mt-xs block h-input w-full shadow-sm rounded cursor-not-allowed bg-gray-100 border border-gray-300"
            value={JSON.stringify(resultSetValue ?? {})}
            disabled
          />
        </label>
      </div>
      <hr />

      {isLoading && <div>Loading...</div>}

      {data && (
        <RemoteSchemaTree
          schema={data}
          relationshipFields={relationshipFields}
          setRelationshipFields={setRelationshipFields}
          fields={fields}
          rootFields={rootFields}
        />
      )}

      {isError && (
        <IndicatorCard status="negative">
          Error loading remote schema
        </IndicatorCard>
      )}
    </div>
  );
};
