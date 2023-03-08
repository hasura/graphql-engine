import React from 'react';
import { Tooltip } from '@hasura/console-legacy-ce';

import styles from '../Metrics.module.scss';
import syncOperationsIcon from '../images/sync.svg';

import LoadingSpinner from '../Common/LoadingSpinner';

/**
 * @typedef Props
 * @property {string} projectName
 * @property {number} networkStatus
 * @property {() => void} refetch
 *
 * @param {Props} props
 */
export const ReloadOperations = props => {
  const { networkStatus, projectName, refetch } = props;
  return (
    <Tooltip
      side="right"
      tooltipContentChildren={
        <span>
          Reload new operations from <code>${projectName}</code>
        </span>
      }
    >
      <span
        className={` ${styles.refetchNewOperations} ${styles.deleteImgMr} ${styles.minHeightIconWrapper} ${styles.displayFlex}`}
      >
        {networkStatus === 4 ? (
          <LoadingSpinner />
        ) : (
          <img
            className={styles.actionImg}
            src={syncOperationsIcon}
            alt="Reload operations"
            onClick={() => {
              refetch();
            }}
          />
        )}
      </span>
    </Tooltip>
  );
};
