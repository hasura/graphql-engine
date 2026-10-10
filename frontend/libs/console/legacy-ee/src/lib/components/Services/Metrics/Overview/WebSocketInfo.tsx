import { Flex, Skeleton } from '@radix-ui/themes';
import { isEmpty } from '../../../../utils/validation';
import styles from '../MetricsV1.module.scss';

const parseDataToString = (data, loading, error, errorLabel = 'data') => {
  return (
    <Skeleton loading={loading ?? false}>
      {error ? `Error fetching ${errorLabel}` : String(data ?? '')}
    </Skeleton>
  );
};

const getHTTPConnCount = (warpThreads = 0, websocket_connections = 0) => {
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
    <div>
      <span className="font-bold mr-2">
        {parseDataToString(count, loading, error, `${label}s`)}
      </span>
      {isEmpty(error) && (
        <span className={styles.mr_sm}>
          {count === 1 ? label : `${label}s`}
        </span>
      )}
    </div>
  );
};

const WebSocketInfo = ({ liveStats = {} as Record<string, any> }) => {
  const { loading = false, error, data = {} } = liveStats;
  const {
    active_subscriptions = 0,
    warp_threads = 0,
    websocket_connections = 0,
  } = data?.search_latest_project_metrics?.[0] || {};

  return (
    <Flex align="center" gap="4">
      <p className="font-bold">Current:</p>
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
    </Flex>
  );
};

export default WebSocketInfo;
