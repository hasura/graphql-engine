import React from 'react';
import TabContainer from '../Containers/TabContainer';
import ActionEditor from './ActionEditor';

const Modify = ({ params, allActions, allTypes, dispatch, ...modifyProps }) => {
  return (
    <TabContainer
      params={params}
      allActions={allActions}
      tabName="modify"
      dispatch={dispatch}
    >
      <ActionEditor
        allActions={allActions}
        allTypes={allTypes}
        dispatch={dispatch}
        actionName={params.actionName}
        {...modifyProps}
      />
    </TabContainer>
  );
};

export default Modify;
