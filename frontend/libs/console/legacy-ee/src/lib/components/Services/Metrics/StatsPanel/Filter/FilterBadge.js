import React from 'react';
import styles from '../../Metrics.module.scss';
import cross from '../../images/x-circle-metrics.svg';

const FilterBadge = props => {
  const { onClick, text } = props;
  return (
    <div className={styles.filterBadge}>
      {text} <img onClick={onClick} src={cross} alt="Cross" />
    </div>
  );
};

export default FilterBadge;
