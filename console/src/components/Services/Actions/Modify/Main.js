import React from 'react';
import ActionContainer from '../Containers/ActionContainer';
import ActionEditor from './ActionEditor';

const Modify = ({ params, allActions, allTypes, dispatch, ...modifyProps }) => (
  <ActionContainer
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
  </ActionContainer>
);

export default Modify;
