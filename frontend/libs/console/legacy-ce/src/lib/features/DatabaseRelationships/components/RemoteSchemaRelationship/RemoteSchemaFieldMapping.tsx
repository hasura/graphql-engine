import {
  buildServerRemoteFieldObject,
  RemoteSchemaTree,
} from './parts/RemoteSchemaTree';
import { RelationshipFields } from './types';
import { GraphQLSchema } from 'graphql';
import React, { useCallback, useEffect, useState } from 'react';
import { RemoteSchemaRelationship } from '../../types';
import { parseServerRelationship } from './utils';
import { RemoteFieldDisplay } from './parts/RemoteFieldDisplay';

interface RemoteSchemaFieldMappingProps {
  graphQLSchema: GraphQLSchema;
  defaultValue?: RemoteSchemaRelationship['definition']['remote_field'];
  onChange?: (
    value: RemoteSchemaRelationship['definition']['remote_field'],
  ) => void;
}

export const RemoteSchemaFieldMapping = (
  props: RemoteSchemaFieldMappingProps,
) => {
  // Defaults live on the destructured parameters rather than on
  // `RemoteSchemaFieldMapping.defaultProps`: React 19 ignores `defaultProps` on
  // function components, so the previous no-op `onChange` default was silently
  // dropped there. `onChange` stays optional in the public prop contract.
  const { defaultValue, onChange = () => undefined, graphQLSchema } = props;

  // Why is this eslint rule disabled? => unless graphQL schema changes there is no change on the parent onChange handler reference.

  const memoizedCallback = useCallback(
    (value: RemoteSchemaRelationship['definition']['remote_field']) =>
      onChange?.(value),
    [graphQLSchema],
  );

  const [relationshipFields, setRelationshipFields] = useState<
    RelationshipFields[]
  >(defaultValue ? parseServerRelationship(defaultValue) : []);

  useEffect(() => {
    memoizedCallback?.(buildServerRemoteFieldObject(relationshipFields));
  }, [memoizedCallback, relationshipFields]);

  return (
    <div>
      <div className="mb-2">
        <RemoteFieldDisplay relationshipFields={relationshipFields} />
      </div>
      <RemoteSchemaTree
        schema={graphQLSchema}
        relationshipFields={relationshipFields}
        setRelationshipFields={setRelationshipFields}
        fields={['AlbumId', 'Title', 'ArtistId']}
        rootFields={['query']}
      />
    </div>
  );
};
