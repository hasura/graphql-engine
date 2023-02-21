import React, { useState } from 'react';
import { Button } from '@hasura/console-legacy-ce';
import { Tooltip } from '@hasura/console-legacy-ce';
import { useQuery, useMutation } from '@apollo/react-hooks';
import { tableScss } from '@hasura/console-legacy-ce';

import styles from '../Metrics.module.scss';
import { ContentWithTestSuite } from './ContentWithTestSuite';
import { fetchAllTestRuns } from './graphql.queries';
import { ActionsPanel } from '../Common/ActionsPanel';
import expand from '../images/expand.svg';
import collapse from '../images/collapse.svg';
import { BrowseTests } from './BrowseTests';
import { getTestRunStatusText, StatusIcon } from './utils';
import { IN_PROCESS_ENUM, QUEUED_ENUM } from './constants';
import { cancelTestRun } from './graphql.queries';

export const PastRuns = props => {
  return (
    <div className={styles.padding_top_20}>
      <ContentWithTestSuite {...props}>
        {({ testSuiteId }) => (
          <PastRunsList {...props} testSuiteId={testSuiteId} />
        )}
      </ContentWithTestSuite>
    </div>
  );
};

/**
 * @typedef Props
 * @property {string} testSuiteId
 *
 * @param {Props} props
 */
const PastRunsList = ({ testSuiteId }) => {
  // TODO: Lets fetch all the test runs for now
  const { data, error, loading, refetch } = useQuery(fetchAllTestRuns, {
    variables: {
      testSuiteId,
      // limit: 5,
      // offset: 0,
    },
    fetchPolicy: 'network-only',
  });

  if (loading) {
    return <span>Fetching past test runs...</span>;
  }

  if (error) {
    return (
      <span className={styles.errorMessage}>
        Error fetching
        <code>{error.toString()}</code>
      </span>
    );
  }

  return (
    <div
      className={`row ${tableScss.add_mar_top_small} col-xs-12  ${styles.addPaddBottom}`}
    >
      {data.test_runs.map((testRun, idx) => (
        <ExpandableRow
          key={testRun.created_at}
          runData={testRun}
          testSuiteId={testSuiteId}
          shouldExpand={idx === 0}
          refetch={refetch}
        />
      ))}
    </div>
  );
};

const ExpandableRow = ({ runData, shouldExpand = false, refetch }) => {
  const [expanded, setExpanded] = useState(shouldExpand);

  const [cancelTest] = useMutation(cancelTestRun, {
    variables: {
      testRunId: runData.id,
    },
    onError: err => {
      alert(err.toString());
    },
    onCompleted: () => {
      refetch();
    },
  });

  const renderCancelButton = () =>
    (runData.status === IN_PROCESS_ENUM || runData.status === QUEUED_ENUM) && (
      <Button
        className={`${styles.placementRight}`}
        onClick={e => {
          e.stopPropagation();
          cancelTest();
        }}
      >
        Cancel
      </Button>
    );

  return (
    <div className={styles.pastRunsContainer}>
      <ActionsPanel
        onClick={() => setExpanded(prev => !prev)}
        className={`${styles.pastRunsActionsPanel} ${styles.cursorPointer}`}
      >
        <div
          className={`${styles.minHeightIconWrapper} ${styles.addPaddingRight}`}
        >
          <Tooltip
            side="top"
            tooltipContentChildren={
              expanded
                ? 'Click to hide the details'
                : 'Click to view the details'
            }
          >
            <img src={expanded ? collapse : expand} alt="expand" />
          </Tooltip>
        </div>
        <StatusIcon status={runData.status} />
        <p className={styles.testRunStatus}>Run #{runData.test_number}</p>
        {getTestRunStatusText(runData)}
        {renderCancelButton()}
      </ActionsPanel>
      {expanded && (
        <div className={styles.pastRunsTableContainer}>
          <BrowseTests testRunId={runData.id} data={runData} />
        </div>
      )}
    </div>
  );
};
