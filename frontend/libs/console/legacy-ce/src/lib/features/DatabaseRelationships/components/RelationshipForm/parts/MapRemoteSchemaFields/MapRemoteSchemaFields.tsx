import {
  buildServerRemoteFieldObject,
  RemoteSchemaTree,
} from './parts/RemoteSchemaTree';
import { RelationshipFields } from './types';
import { GraphQLSchema } from 'graphql';
import { useCallback, useEffect, useState } from 'react';
import { RemoteSchemaRelationship } from '../../../../types';
import { parseServerRelationship } from './utils';
import { Input } from '@hasura/shared/ui';

interface MapRemoteSchemaFieldsProps {
  graphQLSchema: GraphQLSchema;
  defaultValue?: RemoteSchemaRelationship['definition']['remote_field'];
  tableColumns: string[];
  onChange?: (
    value: RemoteSchemaRelationship['definition']['remote_field'],
  ) => void;
}

export const MapRemoteSchemaFields = (props: MapRemoteSchemaFieldsProps) => {
  const { defaultValue, onChange, tableColumns, graphQLSchema } = props;

  // Why is this eslint rule disabled? => unless graphQL schema changes there is no change on the parent onChange handler reference.
  // eslint-disable-next-line react-hooks/exhaustive-deps
  const memoizedCallback = useCallback((v) => onChange?.(v), [graphQLSchema]);

  const [relationshipFields, setRelationshipFields] = useState<
    RelationshipFields[]
  >(defaultValue ? parseServerRelationship(defaultValue) : []);

  useEffect(() => {
    memoizedCallback?.(buildServerRemoteFieldObject(relationshipFields));
  }, [memoizedCallback, relationshipFields]);

  return (
    <div>
      <div className="mb-2">
        <Input
          full
          className="mt-2 cursor-not-allowed"
          disabled
          value={JSON.stringify(
            buildServerRemoteFieldObject(relationshipFields),
          )}
        />
      </div>
      <RemoteSchemaTree
        schema={graphQLSchema}
        relationshipFields={relationshipFields}
        setRelationshipFields={setRelationshipFields}
        fields={tableColumns}
        rootFields={['query']}
      />
    </div>
  );
};
