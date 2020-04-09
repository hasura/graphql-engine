import React from 'react';
import ActionContainer from '../Containers/ActionContainer';
import Codegen from './Codegen';

const CodegenWrapper = ({ params, allActions, dispatch, ...codegenProps }) => {
  return (
    <ActionContainer
      params={params}
      allActions={allActions}
      tabName="codegen"
      dispatch={dispatch}
    >
      <Codegen dispatch={dispatch} allActions={allActions} {...codegenProps} />
    </ActionContainer>
  );
};

export default CodegenWrapper;
