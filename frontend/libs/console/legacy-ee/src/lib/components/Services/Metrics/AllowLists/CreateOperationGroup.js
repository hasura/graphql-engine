import React from 'react';
import { Button } from '@hasura/console-legacy-ce';

import { useMutation } from '@apollo/react-hooks';

import { createOperationGroup } from './graphql.queries';

const CreateOperationGroup = ({ projectId, title, cb }) => {
  const variables = {
    operationInfo: {
      project_id: projectId,
      // name: `${projectId}_default`
      name: 'default',
    },
  };
  const [createOperationGroupMutation, { loading, error }] = useMutation(
    createOperationGroup,
    {
      variables: variables,
      onCompleted: () => cb(),
    }
  );
  const renderBtnText = () => {
    switch (true) {
      case loading:
        return 'Creating ...';
      case typeof error === 'object':
        return error.toString();
      default:
        return title;
    }
  };
  return (
    <Button mode="primary" onClick={() => createOperationGroupMutation()}>
      {renderBtnText()}
    </Button>
  );
};

export default CreateOperationGroup;
