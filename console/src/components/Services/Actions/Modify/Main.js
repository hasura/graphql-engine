import React from 'react';
import TabContainer from '../Containers/TabContainer';

const Modify = ({ params, allActions, dispatch, ...modifyProps }) => {
  console.log(modifyProps);
  return (
    <TabContainer
      params={params}
      allActions={allActions}
      tabName="modify"
      dispatch={dispatch}
    >
      <div> Hello Modify </div>
    </TabContainer>
  );
};

export default Modify;
