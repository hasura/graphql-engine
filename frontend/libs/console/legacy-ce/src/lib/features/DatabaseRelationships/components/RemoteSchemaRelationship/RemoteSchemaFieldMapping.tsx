import {
  buildServerRemoteFieldObject,
  RemoteSchemaTree,
} from './parts/RemoteSchemaTree';
import { RelationshipFields } from './types';
import { GraphQLSchema } from 'graphql';
import React, { useEffect, useRef, useState } from 'react';
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

  const [relationshipFields, setRelationshipFields] = useState<
    RelationshipFields[]
  >(defaultValue ? parseServerRelationship(defaultValue) : []);

  // We only want to notify the parent when the built remote field object
  // changes (i.e. when `relationshipFields` changes), not every time the
  // parent re-renders and hands us a new `onChange` identity. Keeping the
  // latest `onChange` in a ref lets the effect depend solely on
  // `relationshipFields` while still invoking the current handler.
  const onChangeRef = useRef(onChange);
  useEffect(() => {
    onChangeRef.current = onChange;
  });

  useEffect(() => {
    onChangeRef.current?.(buildServerRemoteFieldObject(relationshipFields));
  }, [relationshipFields]);

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
