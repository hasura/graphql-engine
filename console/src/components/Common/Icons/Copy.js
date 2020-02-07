import React from 'react';
import styles from '../Common.scss';

const Copy = ({ className }) => {
  return <i className={`fa fa-copy ${className || ''}`} aria-hidden="true" />;
};

export default Copy;
