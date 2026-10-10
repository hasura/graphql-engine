import { useIntrospectRemoteSchema } from '@hasura/metadata/api';
import {
  RelationshipFields,
  RemoteSchemaTree,
} from '../../../RemoteRelationships';
import { useState } from 'react';

export const SchemaPreview = (props: { name: string }) => {
  const { name } = props;
  const { data, isFetched } = useIntrospectRemoteSchema(name);
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

  if (!data && !isFetched) {
    return <>Error introspecting remote schema</>;
  }

  return data ? (
    <div>
      <div className="group">
        <RemoteSchemaTree
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
