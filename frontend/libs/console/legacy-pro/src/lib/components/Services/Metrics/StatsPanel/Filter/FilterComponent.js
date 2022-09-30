import React from 'react';
import DropdownComponent from './DropdownComponent';

/* Filter component accepts a children prop,
 * Which is a MenuItem instance
 * */
const FilterComponent = props => {
  const styles = require('../../Metrics.scss');
  const { title, children } = props;
  return (
    <div className={styles.selectBox}>
      <span>{title}</span>
      <DropdownComponent {...props}>{children}</DropdownComponent>
    </div>
  );
};
export default FilterComponent;
