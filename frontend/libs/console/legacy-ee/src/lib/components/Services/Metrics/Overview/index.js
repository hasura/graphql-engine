import React, { useEffect, useState } from 'react';
import { Row } from 'react-bootstrap';
import moment from 'moment';
import { useSubscription } from 'react-apollo';
import { Analytics, REDACT_EVERYTHING } from '@hasura/console-legacy-ce';

import { loadInconsistentObjects as loadInconsistentObjectsAction } from '@hasura/console-legacy-ce';

import { isAdmin } from '../utils';
import SourceHealth from './SourceHealth';
import APIHealth from './ApiHealth';
import { fetchLiveStats } from './graphql.queries';
import { refetchMetadata as refetchMetadataAction } from '../../../Main/Actions';
import styles from '../MetricsV1.module.scss';

const Overview = ({
  inconsistentObjects,
  metadata = {},
  refetchMetadata,
  project,
}) => {
  const _isAdmin = isAdmin(project.privileges);

  const projectId = project.id;

  const variables = { projectIds: `{${projectId}}` };
  useEffect(() => {
    // reload metadata when the user comes back to the tab
    if (!metadata.loading && _isAdmin) refetchMetadata();
  }, []);
  const [fromTime, setFromTime] = useState(
    moment().subtract(1, 'hour').toISOString()
  );

  const liveStats = useSubscription(fetchLiveStats, {
    variables,
  });
  return (
    <Analytics name="MonitoringOverview" {...REDACT_EVERYTHING}>
      <div
        className={`${styles.pl_sm} ${styles.pr_sm} ${styles.negativeMT_xl}`}
      >
        <Row className={styles.no_pad} style={{ minHeight: 130 }}>
          <APIHealth
            projectId={projectId}
            liveStats={liveStats}
            fromTime={fromTime}
            setFromTime={setFromTime}
          />
        </Row>
        <hr className="my-md" />
        <Row
          className={`${styles.animated} ${styles.fadeIn} ${styles.sourecHealth_botton_pad}`}
        >
          <SourceHealth
            project={project}
            inconsistentObjects={inconsistentObjects || []}
            metadata={metadata.metadataObject || {}}
          />
        </Row>
      </div>
    </Analytics>
  );
};

const mapStateToProps = (state, ownProps) => {
  const project = state.main.project;
  return {
    ...ownProps,
    project,
    metadata: state.metadata,
    inconsistentObjects: state.metadata.inconsistentObjects,
  };
};
const mapDispatchToProps = dispatch => {
  return {
    refetchMetadata: () => {
      dispatch(refetchMetadataAction());
      dispatch(
        loadInconsistentObjectsAction({
          shouldReloadMetadata: false,
        })
      );
    },
  };
};
const overViewConnector = connect =>
  connect(mapStateToProps, mapDispatchToProps)(Overview);
export default overViewConnector;
