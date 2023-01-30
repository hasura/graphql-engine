import React from 'react';
import DropdownComponent from './DropdownComponent';
import styles from '../../Metrics.module.scss';

/* Filter component accepts a children prop,
 * Which is a MenuItem instance
 * */
const FilterComponent = props => {
  const { title, children } = props;
  return (
    <div className={styles.selectBox}>
      <span>{title}</span>
      <DropdownComponent {...props}>{children}</DropdownComponent>
    </div>
  );
};
export default FilterComponent;
