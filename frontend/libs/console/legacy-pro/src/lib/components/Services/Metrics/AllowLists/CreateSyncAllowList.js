import React, { useState } from 'react';

import CreateAllowList from './CreateAllowList';

import { allowListOperationGroupName } from './constants';

/*
import SyncWithAllowList from './SyncWithAllowList';
*/

import styles from '../Metrics.module.scss';

const CreateSyncAllowList = ({
  projectName,
  projectId,
  refetchALOperationGroup,
}) => {
  const [status, updateStatus] = useState('');

  const onCreateCompleted = () => {
    refetchALOperationGroup();
  };
  /*
  const onSyncError = error => {
    updateStatus(error.toString());
  };
  */

  return (
    <div className={styles.mrTop10}>
      <CreateAllowList
        status={status}
        projectId={projectId}
        updateStatus={updateStatus}
        groupName={allowListOperationGroupName}
        onCreateCompleted={onCreateCompleted}
        projectName={projectName}
      />
      {/*
      <SyncWithAllowList
        status={status}
        projectId={projectId}
        groupName={allowListOperationGroupName}
        onCompleted={onSyncComplete}
        onError={onSyncError}
      />
      */}
    </div>
  );
};

export default CreateSyncAllowList;
