import React from 'react';

import CreateSyncAllowList from './CreateSyncAllowList';

const ManageAllowList = ({
  projectName,
  projectId,
  refetch,
  children,
  shouldCreateOperationGroup,
}) => {
  const renderBody = () => {
    if (shouldCreateOperationGroup) {
      return (
        <CreateSyncAllowList
          projectId={projectId}
          refetchALOperationGroup={refetch}
          projectName={projectName}
        />
      );
    }
    return children();
  };
  return renderBody();
};

export default ManageAllowList;
