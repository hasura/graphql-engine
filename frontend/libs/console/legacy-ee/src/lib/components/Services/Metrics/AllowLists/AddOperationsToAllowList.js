import React, { useState } from 'react';
import { useMutation } from '@apollo/react-hooks';
import { Button } from '@hasura/console-legacy-ce';

import { addToOperationGroup } from '../Operations/graphql.queries';

import { getBulkQuery } from './utils';

import useHasuraQuery from './useHasuraQuery';

import { waitBtnsBeforeReload } from './constants';

import { createCollectionIfNotExist } from './Actions';

import styles from '../Metrics.module.scss';

const defaultState = {
  loading: false,
  erorr: null,
  success: false,
};

const AddOperationToAllowList = ({
  operations,
  collectionName,
  projectId,
  dispatch,
  onErrorCb,
  onCompletedCb,
  btnStates,
}) => {
  const [addOpState, update] = useState(defaultState);
  const { loading, error, success } = addOpState;

  const { btnLoading, btnSuccessful, btnInit } = btnStates;

  const updateKey = ({ key, value }) => {
    update(s => {
      return {
        ...s,
        [key]: value,
      };
    });
  };

  const onError = err => {
    updateKey({
      key: 'loading',
      value: false,
    });
    // Do something here
    updateKey({
      key: 'error',
      value: JSON.stringify(err),
    });
    setTimeout(() => {
      updateKey({
        key: 'error',
        value: null,
      });
    }, waitBtnsBeforeReload);
    onErrorCb(err);
  };
  const onSuccess = data => {
    // Do something
    updateKey({
      key: 'success',
      value: true,
    });
    setTimeout(() => {
      updateKey({
        key: 'success',
        value: false,
      });
      updateKey({
        key: 'loading',
        value: false,
      });
      updateKey({
        key: 'error',
        value: null,
      });
      onCompletedCb(data);
    }, waitBtnsBeforeReload);
  };
  const { refetch: runQuery } = useHasuraQuery({
    dispatcher: dispatch,
    onError: onError,
    onCompleted: onSuccess,
    run: false,
  });
  const getOperationObjs = () => {
    return operations.map(operation => {
      return {
        operation_group_name: collectionName,
        operation_name: operation.name,
        query: operation.query,
        operation_id: operation.operation_id,
        project_id: projectId,
      };
    });
  };
  const variables = {
    insertObj: getOperationObjs(),
  };
  const onCompleted = data => {
    if (data.insert_operation_groups_operations.returning.length >= 1) {
      /* Call the metadata insert api */
      const bulkAllowListInserts =
        data.insert_operation_groups_operations.returning.map(i => {
          return {
            type: 'add_query_to_collection',
            args: {
              collection_name: i.operation_group_name,
              query_name: i.operation_name,
              query: i.query,
            },
          };
        });
      const bulkQuery = getBulkQuery(bulkAllowListInserts);
      runQuery(bulkQuery);
      return;
    }
    alert('Something went wrong! please refresh to continue');
  };
  const onErrorAdding = err => {
    updateKey({
      key: 'loading',
      value: false,
    });
    updateKey({
      key: 'error',
      value: err.toString(),
    });
    setTimeout(() => {
      updateKey({
        key: 'error',
        value: null,
      });
    }, waitBtnsBeforeReload);
  };
  const [addToOperationGroupMutation] = useMutation(addToOperationGroup, {
    variables: variables,
    onCompleted,
    onError: onErrorAdding,
  });
  const initiateFlow = () => {
    updateKey({
      key: 'loading',
      value: true,
    });
    /* Create the collection if it doesn't exist */
    dispatch(createCollectionIfNotExist(collectionName))
      .then(data => {
        if (data.length === 2) {
          addToOperationGroupMutation();
          return;
        }
        onError(new Error('Unexpected error, please report this issue to us'));
      })
      .catch(err => {
        if (
          err &&
          typeof err === 'object' &&
          'code' in err &&
          err.code === 'already-exists'
        ) {
          addToOperationGroupMutation();
        } else {
          onError(err);
        }
      });
  };
  const getBtnText = () => {
    if (loading && !success && !error) {
      return btnLoading;
    }
    if (error && !success) {
      return error;
    }
    if (success) {
      return btnSuccessful;
    }
    return btnInit;
  };
  const getOnClick = () => {
    if (!loading) {
      return initiateFlow;
    }
  };
  return (
    <div className={styles.addOperationsToAllowListBtn}>
      <Button
        mode="primary"
        disabled={!success && loading}
        isLoading={loading}
        onClick={getOnClick()}
      >
        {getBtnText()}
      </Button>
    </div>
  );
};

export default AddOperationToAllowList;
