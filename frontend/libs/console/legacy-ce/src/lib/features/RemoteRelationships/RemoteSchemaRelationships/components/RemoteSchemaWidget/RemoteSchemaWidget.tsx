import React, { useState, useEffect, useRef } from 'react';
import { useFormContext } from 'react-hook-form';

import { IndicatorCard } from '../../../../../new-components/IndicatorCard';
import { useRemoteSchema } from '../../../../MetadataAPI';
import { RemoteRelationship } from '../../../../../metadata/types';

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
import { RelationshipOverview } from './RelationshipOverview';
import { refRemoteOperationSelectorKey } from '../RefRsSelector';

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
  showOnlySelectable?: boolean;
}

const resultSet = 'resultSet';

export const RemoteSchemaWidget = ({
  showOnlySelectable = false,
  schemaName,
  fields,
  rootFields = ['query'],
}: RemoteSchemaWidgetProps) => {
  const { fetchSchema, data, isLoading, isError } = useRemoteSchema();

  const { setValue, watch } = useFormContext<RsToRsSchema>();
  const resultSetValue = watch(resultSet);
  const relationship = watch('relationship');
  const selectedOperation = watch(refRemoteOperationSelectorKey);

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

  // if selected operation is passed from outside, we should
  // skip the first update of relationshipFields. This happens for example
  // in the modify relationship flow
  const skipFirstUpdate = useRef(!!selectedOperation);

  useEffect(() => {
    if (skipFirstUpdate.current) {
      // Skip the effect for the first render
      skipFirstUpdate.current = false;
      return;
    }

    setRelationshipFields([
      {
        key: '__query',
        depth: 0,
        checkable: false,
        argValue: null,
        type: 'field',
      },
      {
        key: `__query.field.${selectedOperation}`,
        depth: 1,
        checkable: false,
        argValue: null,
        type: 'field',
      },
    ]);
  }, [selectedOperation]);

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
          <br />
          <span className="text-gray-600 mb-xs font-normal">
            Build a query mapping from your source schema type to a field
            argument in your reference schema{' '}
          </span>
          <div className="my-xs">
            <RelationshipOverview resultSet={resultSetValue} />
          </div>
          <input
            type="text"
            className="py-6 mt-xs block h-input w-full shadow-sm rounded cursor-not-allowed bg-gray-100 border border-gray-300"
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
          selectedOperation={selectedOperation}
          rootFields={rootFields}
          showOnlySelectable={showOnlySelectable}
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
