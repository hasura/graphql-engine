import React from 'react';
import styles from '../../TableModify/ModifyTable.scss';
import { RemoteRelationshipServer } from './utils';
import RemoteRelationshipList from './components/RemoteRelationshipList';
import { fetchRemoteSchemas } from '../../../RemoteSchema/Actions';

type Props = {
  relationships: RemoteRelationshipServer[];
  reduxDispatch: any;
  table: any;
  remoteSchemas: string[];
};

const RemoteRelationships: React.FC<Props> = ({
  relationships,
  reduxDispatch,
  table,
  remoteSchemas,
}) => {
  React.useEffect(() => {
    reduxDispatch(fetchRemoteSchemas());
  }, []);

  return (
    <div>
      <div className={styles.add_mar_bottom}>
        Relationships to remote schemas
      </div>
      <RemoteRelationshipList
        relationships={relationships}
        table={table}
        remoteSchemas={remoteSchemas}
        reduxDispatch={reduxDispatch}
      />
    </div>
  );
};

export default RemoteRelationships;
