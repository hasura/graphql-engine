import React from 'react';
import { useQuery } from '@apollo/react-hooks';

import { fetchTestSuites } from './graphql.queries';
import { CreateNewTestSuite } from './CreateNewTestSuite';

import styles from '../Metrics.module.scss';

export const ContentWithTestSuite = ({
  projectName,
  projectId,
  children,
  currentProjectId,
}) => {
  const { loading, error, data, refetch } = useQuery(fetchTestSuites, {
    variables: {
      projectId,
    },
    fetchPolicy: 'network-only',
  });

  React.useEffect(() => {
    refetch();
  }, [projectId]);

  const Spanned = ({ children: thisChild, isError }) => {
    return (
      <span
        className={`${styles.displayInline} ${isError && styles.noResultFound}`}
      >
        {thisChild}
      </span>
    );
  };

  const renderBody = () => {
    if (loading) {
      return <Spanned>Getting test_suites...</Spanned>;
    }
    if (error) {
      return <Spanned>Error loading test suites: ${error.toString()}</Spanned>;
    }

    /* Create a new test suite only if required, currentProjectId is the select projectId too */
    if (data && data.test_suites && data.test_suites.length === 0) {
      if (currentProjectId === projectId) {
        return (
          <CreateNewTestSuite
            projectName={projectName}
            projectId={projectId}
            refetch={refetch}
          />
        );
      }
      return <Spanned isError>* No test suite found on this project!</Spanned>;
    }

    return children({
      testSuiteId: data.test_suites[0].id,
      testProjectId: projectId,
    });
  };
  return renderBody();
};
