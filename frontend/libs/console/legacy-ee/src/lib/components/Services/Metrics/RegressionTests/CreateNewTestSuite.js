import React, { useEffect } from 'react';
import { useMutation } from '@apollo/react-hooks';

import { createTestSuite } from './graphql.queries';

export const CreateNewTestSuite = ({ projectId, projectName, refetch }) => {
  const [createNewTestSuite, { loading, error }] = useMutation(
    createTestSuite,
    {
      variables: {
        data: [
          {
            project_id: projectId,
            name: `test-suite-${projectName}`,
          },
        ],
      },
      onCompleted: () => refetch(),
      onError: err => alert(err.toString()),
    }
  );

  useEffect(() => {
    createNewTestSuite();
  }, []);

  if (loading) {
    return <div>Creating new test suite...</div>;
  }

  if (error) {
    return alert(error.toString());
  }

  return null;
};
