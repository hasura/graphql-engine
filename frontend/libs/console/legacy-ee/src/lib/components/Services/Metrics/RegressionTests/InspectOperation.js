import React from 'react';
import { Tooltip } from '@hasura/console-legacy-ce';
import { Link } from 'react-router';

import { createFilter } from '../Error/utils';
import { OPERATION_NAME_SYMBOL, relativeModulePath } from '../constants';
import styles from '../Metrics.module.scss';
import inspectRow from '../images/usage.svg';

/**
 * @typedef Props
 * @property {string} name
 *
 * @param {Props} props
 */
export const InspectOperation = props => {
  const { name } = props;

  const getOperationUrl = () => {
    const filters = [createFilter(OPERATION_NAME_SYMBOL, name)];
    return {
      pathname: `${relativeModulePath}/operations`,
      search: `?filters=${window.encodeURI(JSON.stringify(filters))}`,
    };
  };

  return (
    <div className={styles.iconPadding}>
      <Tooltip side="right" tooltipContentChildren="View operations">
        <Link to={getOperationUrl()}>
          <img
            className={styles.actionImg}
            src={inspectRow}
            alt="Show operations"
          />
        </Link>
      </Tooltip>
    </div>
  );
};
