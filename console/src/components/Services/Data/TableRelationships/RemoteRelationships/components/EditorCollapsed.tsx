import React from 'react';
import { RemoteRelationshipServer } from '../utils';
import styles from '../SchemaExplorer.scss';

type Props = {
  relationship?: RemoteRelationshipServer;
};

const Collapsed: React.FC<Props> = ({ relationship }) => {
  if (!relationship) {
    return null;
  }
  return (
    <div className={styles.display_flex}>
      <div>
        <b>{`${relationship.remote_relationship_name}`}</b>&nbsp;
      </div>
      <div>
        <i>
          {`- ${relationship.table_name} → ${
            relationship.definition.remote_schema
          } . ${Object.keys(relationship.definition.remote_field)}`}
        </i>
      </div>
    </div>
  );
};

export default Collapsed;
