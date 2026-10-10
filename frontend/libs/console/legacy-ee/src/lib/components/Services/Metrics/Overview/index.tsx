import { useState } from 'react';
import { sub } from 'date-fns';
import { useSubscription } from '@apollo/client/react';
import SourceHealth from './SourceHealth';
import APIHealth from './ApiHealth';
import { fetchLiveStats } from './graphql.queries';
import styles from '../MetricsV1.module.scss';
import { useProjectInfo } from '../../../../hooks/useProjectInfo';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';

const Overview = () => {
  const { data: project } = useProjectInfo();
  const [fromTime, setFromTime] = useState(
    sub(new Date(), { hours: 1 }).toISOString(),
  );

  const liveStats = useSubscription(fetchLiveStats, {
    variables: { projectIds: `{${project?.id}}` },
    skip: Boolean(project?.id),
  });
  return (
    <Analytics name="MonitoringOverview" {...REDACT_EVERYTHING}>
      <div
        className={`${styles['pl_sm']} ${styles['pr_sm']} ${styles['negativeMT_xl']}`}
      >
        <div className={styles['no_pad']} style={{ minHeight: 130 }}>
          <APIHealth
            projectId={project?.id}
            liveStats={liveStats}
            fromTime={fromTime}
            setFromTime={setFromTime}
          />
        </div>
        <hr className="my-4" />
        <div
          className={`${styles['animated']} ${styles['fadeIn']} ${styles['sourecHealth_botton_pad']}`}
        >
          <SourceHealth />
        </div>
      </div>
    </Analytics>
  );
};

export default Overview;
