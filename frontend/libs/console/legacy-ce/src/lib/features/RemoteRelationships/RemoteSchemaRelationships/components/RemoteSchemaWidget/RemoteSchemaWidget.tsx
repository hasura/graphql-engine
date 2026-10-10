import { useState, useEffect, useRef } from 'react';
import { useFormContext } from 'react-hook-form';
import { Card, IndicatorCard, JsonCodeBlock, Text } from '@hasura/shared/ui';
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
import { RelationshipOverview } from './RelationshipOverview';
import { refRemoteOperationSelectorKey } from '../RefRsSelector';
import { useIntrospectRemoteSchema } from '@hasura/metadata/api';
import { RemoteRelationship } from '@hasura/shared/types';
import { Skeleton } from '@radix-ui/themes';
import { RsToRsSchema } from '../RemoteSchemaToRemoteSchemaForm/schemas';

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
  serverRelationship,
}: RemoteSchemaWidgetProps) => {
  const { data, isLoading, isError } = useIntrospectRemoteSchema(schemaName);
  const { setValue, watch } = useFormContext<RsToRsSchema>();
  const resultSetValue = watch(resultSet);
  const selectedOperation = watch(refRemoteOperationSelectorKey);

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

  return (
    <Card>
      <div>
        <label className="block">
          <Text weight="bold">Mapping</Text>
          <br />
          <Text>
            Build a query mapping from your source schema type to a field
            argument in your reference schema{' '}
          </Text>
          <div className="my-2">
            <RelationshipOverview resultSet={resultSetValue} />
          </div>
          <JsonCodeBlock
            value={resultSetValue ? JSON.stringify(resultSetValue) : '{}'}
          />
        </label>
      </div>

      {isLoading && <Skeleton height="20px" width="100%" />}

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
    </Card>
  );
};
