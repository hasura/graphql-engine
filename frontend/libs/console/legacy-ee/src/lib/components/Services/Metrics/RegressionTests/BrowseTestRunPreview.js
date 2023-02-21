import React from 'react';
import { useQuery } from '@apollo/react-hooks';

import { tableScss } from '@hasura/console-legacy-ce';
import { fetchRunTestPreview } from './graphql.queries';
import styles from '../Metrics.module.scss';

import { BrowseTests } from './BrowseTests';

/**
 * @typedef Props
 * @property {string} projectId
 * @property {string} testSuiteId
 *
 * @param {Props} props
 */

export const BrowseRunTestsPreview = props => {
  const { testSuiteId, testRunId, projectId } = props;

  const { loading, error, data } = useQuery(fetchRunTestPreview, {
    variables: {
      testSuiteId,
      projectId,
    },
    fetchPolicy: 'network-only',
  });

  if (loading) {
    return (
      <div className={`${tableScss.add_mar_top_small}`}>
        Getting operations from your Hasura account...
      </div>
    );
  }

  if (error) {
    return (
      <div className={`${styles.errorMessage} ${tableScss.add_mar_top_small}`}>
        Error fetching
        <code>{error.message}</code>
      </div>
    );
  }

  const rows = data.results
    ? data.results.map(({ name, role }) => ({
        status: null,
        name,
        role,
        message: undefined,
      }))
    : [];

  return (
    <div
      className={`row ${tableScss.add_mar_top} col-xs-12  ${styles.addPaddBottom}`}
    >
      <BrowseTests data={{ test_run_details: rows }} testRunId={testRunId} />
    </div>
  );
};
