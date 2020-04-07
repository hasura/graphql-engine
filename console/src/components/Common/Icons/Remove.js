import React from 'react';
import styles from '../Common.scss';

const RemoveIcon = ({ className, ...props }) => (
  <i
    className={`${styles.fontAwosomeClose} fa-lg fa fa-times ${className ||
      ''}`}
    {...props}
  />
);

export default RemoveIcon;
