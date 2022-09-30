import React from 'react';
import styles from '../../Metrics.scss';
import cross from '../../images/x-circle.svg';

const FilterBadge = props => {
  const { onClick, text } = props;
  return (
    <div className={styles.filterBadge}>
      {text} <img onClick={onClick} src={cross} alt="Cross" />
    </div>
  );
};

export default FilterBadge;
