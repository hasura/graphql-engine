import React from 'react';

import { Icon } from '../../UIKit/atoms';
import styles from '../Common.scss';

const Check = ({ className }) => {
  return <Icon type="check" className={`${styles.iconCheck} ${className}`} />;
};

export default Check;
