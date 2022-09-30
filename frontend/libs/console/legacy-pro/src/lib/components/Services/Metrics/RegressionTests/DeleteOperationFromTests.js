import React from 'react';

import { DeleteOperations } from '../Common/DeleteOperations';
import { useMutation } from '@apollo/react-hooks';
import { deleteOperationsFromTestSuite } from './graphql.queries';

/**
 * @typedef Props
 * @property {() => void} refetch
 * @property {string} projectId
 * @property {string[]} operationNames
 * @property {string} testSuiteId
 *
 * @param {Props} props
 */
export const DeleteOperationsFromTests = props => {
  const { operationNames, testSuiteId, refetch } = props;

  const [removeFromTestSuite, { loading }] = useMutation(
    deleteOperationsFromTestSuite,
    {
      variables: {
        testSuiteId,
        operationNames,
      },
      onCompleted: () => refetch(),
      onError: err => alert(err.toString()),
    }
  );

  return (
    <DeleteOperations
      onClick={removeFromTestSuite}
      operations={operationNames}
      tooltipText="Remove from regression tests"
      loading={loading}
      altText="Remove operations from regression tests"
    />
  );
};
