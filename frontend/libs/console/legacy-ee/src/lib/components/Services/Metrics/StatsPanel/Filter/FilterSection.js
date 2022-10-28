import React from 'react';
import styles from '../../Metrics.module.scss';

const FilterSection = ({ children }) => {
  return (
    <div className={styles.filterSectionWrapper}>
      <div className={styles.selectBoxWrapper}>{children}</div>
    </div>
  );
};
export default FilterSection;
