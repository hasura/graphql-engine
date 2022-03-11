import React, { useState, useEffect } from 'react';
import { GraphQLSchema } from 'graphql';
import { RemoteRelationship } from '@/metadata/types';
import { Button } from '@/new-components/Button';
import { RemoteSchemaTree } from './RemoteSchemaTree';
import { HasuraColumn, AllowedRootFields, RelationshipFields } from './types';
import { buildServerRemoteFieldObject, parseServerRelationship } from './utils';

export interface RemoteSchemaTreeWrapperProps {
  /**
   * Graphql schema for setting new permissions.
   */
  schema: GraphQLSchema;
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

export const RemoteSchemaTreeWrapper = ({
  schema,
  columns,
  serverRelationship,
  rootFields = ['query'],
}: RemoteSchemaTreeWrapperProps) => {
  const [relationshipFields, setRelationshipFields] = useState<
    RelationshipFields[]
  >([]);

  const [serverRelObject, setServerRelObject] = useState<string>('');

  useEffect(() => {
    if (serverRelationship) {
      setRelationshipFields(parseServerRelationship(serverRelationship));
    }
  }, [serverRelationship]);

  const handleSave = () => {
    setServerRelObject(
      JSON.stringify(
        buildServerRemoteFieldObject(relationshipFields) ?? '',
        null,
        4
      )
    );
  };

  return (
    <div>
      <RemoteSchemaTree
        schema={schema}
        relationshipFields={relationshipFields}
        setRelationshipFields={setRelationshipFields}
        columns={columns}
        rootFields={rootFields}
      />
      <div className="mt-lg">
        <Button onClick={handleSave}>Generate Relationship Object</Button>
      </div>
      <div className="mt-lg">
        <code className="whitespace-pre-wrap">{serverRelObject}</code>
      </div>
    </div>
  );
};
