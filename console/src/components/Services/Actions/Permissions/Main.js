import React from 'react';
import ActionContainer from '../Containers/ActionContainer';

const Permissions = ({ params, allActions, dispatch, ...permissionProps }) => {
  console.log(permissionProps);
  return (
    <ActionContainer
      params={params}
      allActions={allActions}
      tabName="permissions"
      dispatch={dispatch}
    >
      <div> Hello Permissions </div>
    </ActionContainer>
  );
};

export default Permissions;
