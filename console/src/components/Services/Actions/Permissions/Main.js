import React from 'react';
import TabContainer from '../Containers/TabContainer';

const Permissions = ({ params, allActions, dispatch, ...permissionProps }) => {
  console.log(permissionProps);
  return (
    <TabContainer
      params={params}
      allActions={allActions}
      tabName="permissions"
      dispatch={dispatch}
    >
      <div> Hello Permissions </div>
    </TabContainer>
  );
};

export default Permissions;
