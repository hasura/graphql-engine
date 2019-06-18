import React, { useEffect } from 'react';
import styles from '../../TableModify/ModifyTable.scss';
import { parseRemoteRelationship } from '../utils';
import { setRemoteRelationships, defaultRemoteRelationship } from '../Actions';
import RemoteRelationshipEditor from './RemoteRelationshipEditor';

const RemoteRelationships = ({
  remoteRelationships,
  dispatch,
  tableSchema,
  remoteSchemas,
}) => {
  const existingRemoteRelationships = tableSchema.remote_relationships.map(
    rr => {
      return parseRemoteRelationship({
        remote_schema: rr.configuration.remote_schema,
        remote_field: rr.configuration.remote_field,
        name: rr.configuration.name,
      });
    }
  );
  existingRemoteRelationships.push({
    ...defaultRemoteRelationship,
  });
  useEffect(() => {
    dispatch(setRemoteRelationships(existingRemoteRelationships));
  }, []);

  const relationshipList = () => {
    const numRels = remoteRelationships.relationships.length;
    return remoteRelationships.relationships.map((remoteRelationship, i) => {
      return (
        <RemoteRelationshipEditor
          relationship={remoteRelationship}
          allRelationships={remoteRelationships.relationships}
          index={i}
          numRels={numRels}
          dispatch={dispatch}
          tableSchema={tableSchema}
          remoteSchemas={remoteSchemas}
          loading={remoteRelationships.loading}
          key={
            existingRemoteRelationships[i]
              ? existingRemoteRelationships[i].name
              : 'new-remote-rel'
          }
        />
      );
    });
  };

  return (
    <div>
      <div className={styles.add_mar_bottom}>
        Add relationship to a remote schema
      </div>
      <div>{relationshipList()}</div>
    </div>
  );
};

export default RemoteRelationships;

/*
  1. In handleRemoteSchemaChange, whenever a remote schmea is changed, introspect the remote schema and store in state
  2. Onsave, create the remote relationship
  3. OnDelte, delete the remote relationship
  4. OnLoad, serialise the remote relationships into state
  5. OnEdit of remote schema, introspect
*/
