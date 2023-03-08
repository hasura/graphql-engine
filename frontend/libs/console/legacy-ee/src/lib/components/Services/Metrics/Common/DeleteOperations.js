import React from 'react';
import { Tooltip } from '@hasura/console-legacy-ce';

import styles from '../Metrics.module.scss';
import deleteIcon from '../images/delete.svg';

import LoadingSpinner from '../Common/LoadingSpinner';

/**
 * @typedef Props
 * @property {() => void} onClick
 * @property {boolean} loading
 * @property {string} tooltipText
 * @property {string} altText
 *
 * @param {Props} props
 */
const DeleteIcon = ({ loading, tooltipText, onClick, altText }) => {
  if (loading) {
    return <LoadingSpinner />;
  }

  return (
    <Tooltip side="right" tooltipContentChildren={tooltipText}>
      <img src={deleteIcon} alt={altText} onClick={onClick} />
    </Tooltip>
  );
};

/**
 * @typedef Props
 * @property {() => void} onClick
 * @property {string[]} operations
 * @property {boolean} loading
 * @property {string} tooltipText
 * @property {string} altText
 *
 * @param {Props} props
 */
export const DeleteOperations = ({
  onClick,
  operations,
  loading,
  tooltipText,
  altText,
}) => (
  <div
    className={`
      ${styles.displayFlex} ${styles.minHeightIconWrapper} ${
      styles.deleteImgMr
    } ${operations.length ? styles.deleteImg : styles.disabledBtn}
      `}
  >
    <DeleteIcon
      onClick={operations.length ? onClick : undefined}
      loading={loading}
      tooltipText={tooltipText}
      altText={altText}
    />
  </div>
);
