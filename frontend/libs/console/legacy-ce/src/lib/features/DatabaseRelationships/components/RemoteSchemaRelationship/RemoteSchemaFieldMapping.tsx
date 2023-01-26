/* eslint-disable no-restricted-imports */
/* eslint-disable import/first */
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
    value: RemoteSchemaRelationship['definition']['remote_field']
  ) => void;
}

export const RemoteSchemaFieldMapping = (
  props: RemoteSchemaFieldMappingProps
) => {
  const { defaultValue, onChange, graphQLSchema } = props;

  // Why is this eslint rule disabled? => unless graphQL schema changes there is no change on the parent onChange handler reference.
  // eslint-disable-next-line react-hooks/exhaustive-deps
  const memoizedCallback = useCallback(
    (value: RemoteSchemaRelationship['definition']['remote_field']) =>
      onChange?.(value),
    [graphQLSchema]
  );

  const [relationshipFields, setRelationshipFields] = useState<
    RelationshipFields[]
  >(defaultValue ? parseServerRelationship(defaultValue) : []);

  useEffect(() => {
    memoizedCallback?.(buildServerRemoteFieldObject(relationshipFields));
  }, [memoizedCallback, relationshipFields]);

  return (
    <div>
      <div className="mb-sm">
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

RemoteSchemaFieldMapping.defaultProps = {
  defaultValue: undefined,
  onChange: () => {},
};
