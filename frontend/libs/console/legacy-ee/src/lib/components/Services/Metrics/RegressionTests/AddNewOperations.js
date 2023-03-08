import React, { useState } from 'react';

import { useMutation } from '@apollo/react-hooks';
import { Button } from '@hasura/console-legacy-ce';
import { insertTestSuiteOperations } from './graphql.queries';
import styles from '../Metrics.module.scss';

const btnStates = {
  loading: 'Adding...',
  success: 'Added Successfully',
  init: 'Add to test suite',
};

/**
 * @typedef Props
 * @property {string} projectId
 * @property {{ name: string, operation_id: string }[]} operations
 * @property {string} testSuiteId
 * @property {() => void} onComplete
 *
 * @param {Props} props
 */
export const AddNewOperations = props => {
  const { operations = [], testSuiteId, projectId, onComplete } = props;
  const [state, setState] = useState('INIT');

  const [insertOperations, { loading }] = useMutation(
    insertTestSuiteOperations,
    {
      variables: {
        operations: operations.map(o => ({
          operation_name: o.name,
          operation_id: o.operation_id,
          query: o.query,
          variables: (o.http_logs.length && o.http_logs[0].variables) || {},
          user_vars:
            (o.http_logs.length && o.http_logs[0].session_variables) || {},
          test_suite_id: testSuiteId,
          project_id: projectId,
        })),
      },
      onError: err => {
        alert(err.toString());
      },
      onCompleted: () => {
        setState('SUCCESS');
        onComplete();
      },
    }
  );

  const getText = () => {
    if (loading) {
      return btnStates.loading;
    }
    if (state === 'SUCCESS') {
      return setInterval(() => btnStates.success, 1000);
    }
    return btnStates.init;
  };

  return (
    <Button
      size="sm"
      className={`${styles.addOperationsBtn}`}
      onClick={insertOperations}
    >
      {getText()}
    </Button>
  );
};
