import React from 'react';

import { Icon } from '../../UIKit/atoms';
import styles from '../Common.scss';

const Cross = ({ className }) => {
  return <Icon type="close" className={`${styles.iconCross} ${className}`} />;
};

export default Cross;
