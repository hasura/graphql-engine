import React, { useMemo } from 'react';
import { useQuery } from 'react-apollo';
import { FaDatabase } from 'react-icons/fa';
import { addNumbersSafely, isEmptyObject } from '../../../../utils/validation';
import styles from '../MetricsV1.module.scss';
import {
  fetchDbMetrics,
  fetchDbReplicaMetrics,
  fetchDbMasterDBMetrics,
} from './graphql.queries';

const getQueryVariables = ({
  source = {},
  projectId,
  from = new Date(new Date().getTime() - 60 * 1000).toISOString(),
}) => {
  // fixme: add support to non PG Dbs once the spec is ready, rn there is no DB metrics available

  // Read Replica from read replica group
  if (source?.isReadReplica) {
    return [
      fetchDbReplicaMetrics,
      {
        variables: {
          from,
          projectId,
          name: source?.sourceName ?? '',
        },
      },
    ];
  }
  // master DB from read replica group
  if (source?.hasReadReplica) {
    return [
      fetchDbMasterDBMetrics,
      {
        variables: {
          from,
          projectId,
          db: source?.name ?? '',
        },
      },
    ];
  }
  // DB without replicas
  return [
    fetchDbMetrics,
    {
      variables: {
        from,
        projectId,
        db: source?.name ?? '',
      },
    },
  ];
};

const DatabaseInfoCard = ({ selectedSource: source = {}, projectId }) => {
  // avoid refetch on every re-render
  const queryVars = useMemo(
    () =>
      getQueryVariables({
        source,
        projectId,
      }),
    [source]
  );
  const { loading, error, data } = useQuery(...queryVars);
  let totalConnections = 0;
  if (source?.isReadReplica) {
    const totalsMap = data?.database_metrics.reduce((a, c) => {
      return {
        ...a,
        [c.database_host]: addNumbersSafely(
          a?.database_host ?? 0,
          Number(c.total_connections)
        ),
      };
    }, {});
    totalConnections =
      Object.values(totalsMap ?? {})?.[(source?.ix || 1) - 1] ?? 0;
  } else {
    totalConnections = data?.database_metrics.reduce(
      (a, c) => addNumbersSafely(a, Number(c.total_connections)),
      Number(0)
    );
  }

  if (!source || isEmptyObject(source)) {
    return (
      <div id="placeholder" className={`${styles.placeholder_outline}`}>
        Select a source for more information
      </div>
    );
  }
  const poolSettings =
    source?.pool_settings || // for read replicas
    source?.configuration?.connection_info?.pool_settings || // for other sources
    {};

  let maxConnections;
  if (poolSettings?.max_connections && data?.database_metrics?.length) {
    maxConnections =
      poolSettings?.max_connections * data?.database_metrics?.length;
  }

  return (
    <div className={`${styles.card} ${styles.shadow}`}>
      <p className={`${styles.strong} ${styles.mb_xs}`}>
        <FaDatabase className={styles.mr_xxs} aria-hidden="true" />
        {source?.name || ''}
      </p>
      <div>
        <table className={styles.wd100Percent}>
          <tbody>
            <tr>
              <td />
              <td />
            </tr>
            <tr>
              <td className={styles.strong}>Connection Pool Limit</td>
              <td className={styles.right}>
                {maxConnections && !isNaN(maxConnections)
                  ? maxConnections
                  : 'DEFAULT'}
              </td>
            </tr>
            <tr>
              <td className={styles.strong}>Current Connections</td>
              <td className={styles.right}>
                {loading && !error && 'Loading..'}
                {error && 'Error fetching info'}
                {data && (totalConnections || Number(0))}
              </td>
            </tr>
          </tbody>
        </table>
      </div>
    </div>
  );
};

export default DatabaseInfoCard;
