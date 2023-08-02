import React from 'react';

import { deleteOperationsFromAllowListQuery } from './utils';

import useHasuraQuery from './useHasuraQuery';
import { DeleteOperations } from '../Common/DeleteOperations';

const DeleteOperationsFromAllowList = ({
  operationGroupName,
  operationNames,
  refetch,
  dispatch,
  onCompleted: executeOnComplete,
}) => {
  const deleteOperationQuery = deleteOperationsFromAllowListQuery({
    operationNames,
    collectionName: operationGroupName,
  });

  const onOperationFromMetadataDeleteCompleted = () => {
    refetch();
    executeOnComplete();
  };

  const { loading: deleteMetadataInProgress, refetch: runDelete } =
    useHasuraQuery({
      query: deleteOperationQuery,
      dispatcher: dispatch,
      onError: err => alert(JSON.stringify(err)),
      onCompleted: onOperationFromMetadataDeleteCompleted,
      run: false,
    });

  return (
    <DeleteOperations
      onClick={runDelete}
      operations={operationNames}
      tooltipText="Remove from allow list"
      loading={deleteMetadataInProgress}
      altText="Remove operations from allow list"
    />
  );
};

export default DeleteOperationsFromAllowList;
