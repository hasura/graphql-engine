import React from 'react';
import PropTypes from 'prop-types';
import styles from '../Common/Common.module.scss';

const PlaceHolder = ({ width = 5 }) => {
  return (
    <span className={`${styles.placeholder} ${styles.noselect}`}>
      {new Array(width).fill('-').join('')}
    </span>
  );
};

PlaceHolder.propTypes = {
  width: PropTypes.number,
};

export default PlaceHolder;
