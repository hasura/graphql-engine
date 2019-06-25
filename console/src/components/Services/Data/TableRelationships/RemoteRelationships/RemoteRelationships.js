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
  // restructure existing relationships to the state structure
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
  }, [tableSchema.remote_relationships]);

  // render list of relationships
  const relationshipList = () => {
    const numRels = remoteRelationships.relationships.length;
    return remoteRelationships.relationships.map((remoteRelationship, i) => {
      return (
        <RemoteRelationshipEditor
          relationship={remoteRelationship}
          allRelationships={remoteRelationships.relationships}
          existingRelationship={existingRemoteRelationships[i]}
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
        Relationships to remote schemas
      </div>
      <div>{relationshipList()}</div>
    </div>
  );
};

export default RemoteRelationships;
