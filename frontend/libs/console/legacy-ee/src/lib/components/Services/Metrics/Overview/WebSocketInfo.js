import React from 'react';
import { isEmpty } from '../../../../utils/validation';
import Placeholder from '../../../Placeholder/Placeholder';

import styles from '../MetricsV1.module.scss';

const parseDataToString = (data, loading, error, errorLabel = 'data') => {
  if (loading) return <Placeholder width={5} />;
  if (data || Number.isInteger(data)) return data + '';
  if (error !== undefined) return `Error fetching ${errorLabel}`;
  return '';
};

const getHTTPConnCount = (warpThreads = 0, websocket_connections) => {
  // count - websockets
  if (
    websocket_connections &&
    warpThreads &&
    !isNaN(warpThreads) &&
    !isNaN(websocket_connections)
  ) {
    return Number(warpThreads) - Number(websocket_connections);
  }
  // no subscriptions
  if (warpThreads && !isNaN(warpThreads)) return Number(warpThreads);

  return 0;
};

// NOTE: "s" is suffixed to make plural of label
const InfoItem = ({ count, error, loading, label }) => {
  return (
    <>
      <p className={`${styles.mr_xxs} ${styles.strong}`}>
        {parseDataToString(count, loading, error, `${label}s`)}
      </p>
      {isEmpty(error) && (
        <p className={styles.mr_sm}>{count === 1 ? label : `${label}s`}</p>
      )}
    </>
  );
};

const WebSocketInfo = ({ liveStats = {} }) => {
  const { loading, error, data = {} } = liveStats;
  const {
    active_subscriptions = 0,
    warp_threads = 0,
    websocket_connections = 0,
  } = data?.search_latest_project_metrics?.[0] || {};

  return (
    <>
      <p className={`${styles.mr_sm} ${styles.strong} ${styles.flexPullRight}`}>
        Current:
      </p>
      {isEmpty(error) ? (
        <>
          <InfoItem
            count={getHTTPConnCount(warp_threads - websocket_connections)}
            loading={loading}
            error={error}
            label="HTTP Connection"
          />
          <InfoItem
            count={active_subscriptions}
            loading={loading}
            error={error}
            label="Active Subscription"
          />
          <InfoItem
            count={websocket_connections}
            loading={loading}
            error={error}
            label="Open Websocket"
          />
        </>
      ) : (
        <p className={styles.mr_sm}>Error fetching data.</p>
      )}
    </>
  );
};

export default WebSocketInfo;
