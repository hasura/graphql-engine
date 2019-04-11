import React, { useEffect } from 'react';
import AddRemoteRelationship from './AddRemoteRelationship';
import ListRemoteRelationships from './ListRemoteRelationships';
import styles from '../../TableModify/ModifyTable.scss';
import { loadRemoteRelationships } from './remoteRelationshipUtils';

const RemoteRelationships = props => {
  // const remoteRels = props.remoteRels;
  useEffect(() => {
    props.dispatch(loadRemoteRelationships(props.tableSchema.table_name));
  }, []);
  const noRemoteRelsMessage = (
    <div className={styles.activeEdit}>
      <div className={`${styles.remove_margin_bottom} form-group`}>
        <label>
          {' '}
          This table does not have any relationship with any remote schema{' '}
        </label>
      </div>
    </div>
  );

  const remoteRelList = <ListRemoteRelationships {...props} />;

  const { remoteRels } = props;
  const remoteRelContent =
    remoteRels.length > 0 ? remoteRelList : noRemoteRelsMessage;

  return (
    <div>
      {remoteRelContent}
      <AddRemoteRelationship {...props} />
    </div>
  );
};

export default RemoteRelationships;
