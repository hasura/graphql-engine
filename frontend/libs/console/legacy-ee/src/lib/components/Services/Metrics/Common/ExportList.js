import React from 'react';
import { Tooltip } from '@hasura/console-legacy-ce';

import styles from '../Metrics.module.scss';
import downloadList from '../images/download_bottom.svg';

import LoadingSpinner from './LoadingSpinner';

/**
 * @typedef Props
 * @property {boolean} loading
 * @property {() => void} onClick
 * @property {string} altText
 *
 * @param {Props} props
 */
export const ExportList = ({ loading, onClick, altText }) => {
  return (
    <div
      className={`${styles.iconPadding} ${styles.minHeightIconWrapper} ${styles.deleteImgMr} ${styles.commonIcon} ${styles.displayFlex}`}
    >
      {loading ? (
        <LoadingSpinner />
      ) : (
        <Tooltip side="right" tooltipContentChildren="Export">
          <img
            className={styles.actionImg}
            src={downloadList}
            alt={altText}
            onClick={onClick}
          />
        </Tooltip>
      )}
    </div>
  );
};
