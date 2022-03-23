import React, { useState, useEffect } from 'react';
import { useFormContext } from 'react-hook-form';

import { Select } from '@/new-components/Form';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import { useRemoteSchema } from '@/features/MetadataAPI';
import { RemoteRelationship } from '@/metadata/types';

import {
  RemoteSchemaTree,
  buildServerRemoteFieldObject,
  parseServerRelationship,
} from '../RemoteSchemaTree';

import {
  HasuraColumn,
  AllowedRootFields,
  RelationshipFields,
} from '../../types';

export interface RemoteSchemaWidgetProps {
  type: 'from' | 'to';
  schemaList: string[];
  /**
   * Columns array from the current table.
   */
  columns: HasuraColumn;
  /**
   * Remote relationship object from server, for already present permissions.
   * This will be parsed and tree will be populated accordingly
   */
  serverRelationship?: RemoteRelationship;
  rootFields?: AllowedRootFields;
}

export const RemoteSchemaWidget = ({
  type,
  schemaList,
  columns,
  serverRelationship,
  rootFields = ['query'],
}: RemoteSchemaWidgetProps) => {
  const name = type === 'from' ? 'From' : 'To';
  const schemaName = `remoteSchema${name}`;
  const resultSet = `resultSet${name}`;

  const { fetchSchema, data, isLoading, isError } = useRemoteSchema();

  const { setValue, watch } = useFormContext();
  const schemaNameValue = watch(schemaName);
  const resultSetValue = watch(resultSet);

  const [relationshipFields, setRelationshipFields] = useState<
    RelationshipFields[]
  >([]);

  useEffect(() => {
    if (serverRelationship) {
      setValue(schemaName, serverRelationship.name);
      setRelationshipFields(parseServerRelationship(serverRelationship));
    }
  }, [serverRelationship, setValue, schemaName]);

  useEffect(() => {
    const value = buildServerRemoteFieldObject(relationshipFields);
    const jsonValue = JSON.stringify(value ?? '');
    setValue(`resultSet${name}`, jsonValue);
  }, [relationshipFields, setValue, name]);

  useEffect(() => {
    if (schemaNameValue) {
      fetchSchema(schemaNameValue);
    }
  }, [fetchSchema, schemaNameValue]);

  return (
    <div className="grid gap-4 border border-gray-300 rounded shadow-sm p-4 bg-gray-50">
      <div className="grid gap-4 w-96">
        <Select
          label="Remote Schema"
          disabled={type === 'from'}
          name={`remoteSchema${name}`}
          placeholder="Select remote schema"
          options={schemaList.map(value => ({
            label: value,
            value,
          }))}
        />
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

      {data && (
        <RemoteSchemaTree
          schema={data}
          relationshipFields={relationshipFields}
          setRelationshipFields={setRelationshipFields}
          columns={columns}
          rootFields={rootFields}
        />
      )}

      {isError && (
        <IndicatorCard status="negative">
          Error loading remote schemas
        </IndicatorCard>
      )}
    </div>
  );
};
