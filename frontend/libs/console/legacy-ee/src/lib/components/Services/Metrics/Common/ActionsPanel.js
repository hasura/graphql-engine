import React from 'react';

import styles from '../Metrics.module.scss';

/**
 * @typedef Props
 * @property {JSX.Element[]} children
 *
 * @param {Props} props
 *
 * @example
 * return (
 *  <ActionsPanel>
 *   <AddItem />
 *  </ActionsPanel>
 * )
 */
export const ActionsPanel = props => {
  const { children, className, ...rest } = props;

  let classNames = `${styles.boxShadow} ${styles.lightGrayBgColor} ${styles.selectSection}`;
  if (className) {
    classNames = `${classNames} ${className}`;
  }

  // no need to render ActionsPanel if there are no actions
  return children ? (
    <div className={classNames} {...rest}>
      {children}
    </div>
  ) : null;
};
