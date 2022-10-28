import React from 'react';
import { RemoteRelationshipServer, parseRemoteRelationship } from '../utils';
import styles from '../SchemaExplorer.module.scss';

type Props = {
  relationship?: RemoteRelationshipServer;
};

const Collapsed: React.FC<Props> = ({ relationship }) => {
  if (!relationship) {
    return null;
  }
  const parseRelationship = parseRemoteRelationship(relationship);
  const relationMap = () => {
    const fields = parseRelationship.remoteFields.map(field => field.name);
    return ['', ...fields].join(' . ');
  };
  return (
    <div className={styles.display_flex}>
      <div>
        <b>{`${relationship.remote_relationship_name}`}</b>&nbsp;
      </div>
      <div>
        <i>
          {`- ${relationship.table_name} → ${
            parseRelationship.remoteSchema
          }  ${relationMap()}`}
        </i>
      </div>
    </div>
  );
};

export default Collapsed;
