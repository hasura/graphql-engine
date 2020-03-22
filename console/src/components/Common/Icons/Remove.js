import React from 'react';

import { Icon } from '../../UIKit/atoms';
import styles from '../Common.scss';

const RemoveIcon = ({ className, ...props }) => (
  <Icon
    type="close"
    {...props}
    className={`${styles.fontAwosomeClose} ${className || ''}`}
  />
);

export default RemoveIcon;
