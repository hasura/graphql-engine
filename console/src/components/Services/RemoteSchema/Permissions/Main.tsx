import React from 'react';
import Permissions from './Permissions';
import RemoteSchemaContainer from '../Containers/remoteSchemaContainer';

const PermWrapper = ({
  params,
  allRemoteSchemas,
  allRoles,
  dispatch,
  ...permissionProps
}) => {
  return (
    <RemoteSchemaContainer
      params={params}
      allRemoteSchemas={allRemoteSchemas}
      tabName="permissions"
      dispatch={dispatch}
    >
      <Permissions
        allRoles={allRoles}
        dispatch={dispatch}
        {...permissionProps}
      />
    </RemoteSchemaContainer>
  );
};

export default PermWrapper;
