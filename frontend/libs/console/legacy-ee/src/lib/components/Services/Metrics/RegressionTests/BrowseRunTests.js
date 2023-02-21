import React, { Fragment } from 'react';
import { useSubscription } from '@apollo/react-hooks';

import { tableScss } from '@hasura/console-legacy-ce';
import { subscribeTestRunDetails } from './graphql.queries';
import styles from '../Metrics.module.scss';
import { ActionsPanel } from '../Common/ActionsPanel';

import { BrowseTests } from './BrowseTests';

import LoadingSpinner from '../Common/LoadingSpinner';

import moment from 'moment';
import { getTestRunStatusText, StatusIcon } from './utils';
import { IN_PROCESS_ENUM } from './constants';

/**
 * @typedef Props
 * @property {string} projectId
 * @property {string} testSuiteId
 * @property {string} testRunId
 * @property {Date} createdAt
 *
 * @param {Props} props
 */

export const BrowseRunTests = props => {
  const { testRunId } = props;

  const { loading, error, data } = useSubscription(subscribeTestRunDetails, {
    variables: {
      testRunId,
    },
  });

  const getRunningTimeText = createdAt => {
    return `Started ${moment(new Date(createdAt)).fromNow()}`;
  };

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

  const renderRunnerResult = () => {
    if (data.test_runs && data.test_runs.length > 0) {
      const testRunsInfo = data.test_runs[0];
      const renderSpinner = () => {
        return <LoadingSpinner />;
      };

      const running = testRunsInfo.status === IN_PROCESS_ENUM;
      return (
        <Fragment>
          <ActionsPanel>
            <StatusIcon status={testRunsInfo.status} />
            <p className={styles.testRunStatus}>
              Run #
              {(testRunsInfo.test_number && testRunsInfo.test_number) ||
                renderSpinner()}
            </p>
            {running
              ? getRunningTimeText(testRunsInfo.created_at)
              : getTestRunStatusText(testRunsInfo)}
          </ActionsPanel>
          <BrowseTests data={testRunsInfo} testRunId={testRunId} />
        </Fragment>
      );
    }
  };

  return (
    <div
      className={`row ${tableScss.add_mar_top_small} col-xs-12  ${styles.addPaddBottom}`}
    >
      {renderRunnerResult()}
    </div>
  );
};
