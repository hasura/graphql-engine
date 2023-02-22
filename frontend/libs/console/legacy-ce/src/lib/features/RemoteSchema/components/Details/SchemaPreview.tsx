import { useRemoteSchema } from '../../../MetadataAPI';
import {
  RelationshipFields,
  RemoteSchemaTree,
} from '../../../RemoteRelationships';
import React, { useEffect, useState } from 'react';

export const SchemaPreview = (props: { name: string }) => {
  const { name } = props;
  const { fetchSchema, data } = useRemoteSchema();
  const [relationshipFields, setRelationshipFields] = useState<
    RelationshipFields[]
  >([
    {
      key: '__query',
      depth: 0,
      checkable: false,
      argValue: null,
      type: 'field',
    },
  ]);

  useEffect(() => {
    if (name) {
      fetchSchema(name);
    }
  }, [fetchSchema, name]);

  if (!data) {
    return <>Error introspecting remote schema</>;
  }

  return data ? (
    <div>
      <div className="group">
        <RemoteSchemaTree
          className="group-first:bg-gray-50"
          checkable={false}
          schema={data}
          fields={['query']}
          relationshipFields={relationshipFields}
          setRelationshipFields={setRelationshipFields}
          rootFields={['query', 'mutation', 'subscription']}
        />
      </div>
    </div>
  ) : null;
};
