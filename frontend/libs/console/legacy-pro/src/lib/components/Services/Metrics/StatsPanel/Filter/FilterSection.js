import React from 'react';

const FilterSection = ({ children }) => {
  const styles = require('../../Metrics.scss');
  return (
    <div className={styles.filterSectionWrapper}>
      <div className={styles.selectBoxWrapper}>{children}</div>
    </div>
  );
};
export default FilterSection;
