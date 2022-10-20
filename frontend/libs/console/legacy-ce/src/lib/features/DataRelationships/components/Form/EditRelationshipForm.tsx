import React from 'react';
import { Driver } from '@/dataSources';
import { DataTarget } from '@/features/Datasources';
import { DbToRsForm } from '@/features/RemoteRelationships';
import {
  useFindRelationship,
  Relationship,
} from '@/features/RelationshipsTable';
import {
  isLegacyRemoteSchemaRelationship,
  isRemoteDBRelationship,
  isRemoteSchemaRelationship,
} from '@/features/DataSource';
import { RemoteDBRelationshipWidget } from '../RemoteDBRelationshipWidget';
import { LocalRelationshipWidget } from '../LocalDBRelationshipWidget';

type EditRelationshipFormProps = {
  driver: Driver;
  sourceTableInfo: DataTarget;
  existingRelationship: Relationship;
  /**
   * optional callback function, can be used to get the onComplete event, this could be a onSuccess, or onError event.
   *
   */
  onComplete: (callback: {
    title?: string;
    message?: string;
    type: 'success' | 'error' | 'cancel';
  }) => void;
};

export const EditRelationshipForm = ({
  sourceTableInfo,
  driver,
  existingRelationship,
  onComplete,
}: EditRelationshipFormProps) => {
  const { data: relationship, isLoading } = useFindRelationship({
    dataSourceName: existingRelationship.mapping.from.source,
    table: existingRelationship.mapping.from.table,
    relationshipName: existingRelationship.name,
  });
  if (isLoading) return <>Loading...</>;

  if (!relationship) return <>Relationship Not found in metadata</>;

  if (isRemoteDBRelationship(relationship)) {
    return (
      <RemoteDBRelationshipWidget
        key={relationship.name}
        sourceTableInfo={sourceTableInfo}
        existingRelationshipName={relationship.name}
        onComplete={onComplete}
      />
    );
  }

  if (isRemoteSchemaRelationship(relationship)) {
    const { lhs_fields, remote_field, remote_schema } = {
      ...relationship.definition.to_remote_schema,
    };

    return (
      <DbToRsForm
        key={relationship.name}
        sourceTableInfo={sourceTableInfo}
        onComplete={onComplete}
        selectedRelationship={{
          name: relationship.name,
          definition: {
            to_remote_schema: {
              lhs_fields,
              remote_field,
              remote_schema,
            },
          },
        }}
      />
    );
  }

  if (isLegacyRemoteSchemaRelationship(relationship)) {
    const { lhs_fields, remote_field, remote_schema } = {
      ...relationship.definition,
      lhs_fields: relationship.definition.hasura_fields,
    };

    return (
      <DbToRsForm
        key={relationship.name}
        sourceTableInfo={sourceTableInfo}
        onComplete={onComplete}
        selectedRelationship={{
          name: relationship.name,
          definition: {
            to_remote_schema: {
              lhs_fields,
              remote_field,
              remote_schema,
            },
          },
        }}
      />
    );
  }
  return (
    <LocalRelationshipWidget
      key={relationship.name}
      driver={driver}
      sourceTableInfo={sourceTableInfo}
      existingRelationshipName={relationship.name}
      onComplete={onComplete}
    />
  );
};
