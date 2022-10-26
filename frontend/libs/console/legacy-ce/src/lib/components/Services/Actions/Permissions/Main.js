import React from 'react';
import ActionContainer from '../Containers/ActionContainer';
import Permissions from './Permissions';

const PermWrapper = ({
  params,
  allActions,
  allRoles,
  dispatch,
  ...permissionProps
}) => {
  return (
    <ActionContainer
      params={params}
      allActions={allActions}
      tabName="permissions"
      dispatch={dispatch}
    >
      <Permissions
        allRoles={allRoles}
        dispatch={dispatch}
        {...permissionProps}
      />
    </ActionContainer>
  );
};

export default PermWrapper;
