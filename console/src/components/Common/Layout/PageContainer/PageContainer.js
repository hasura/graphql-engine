import React from 'react';
import Helmet from 'react-helmet';

import styles from '../../Common.scss';

const PageContainer = ({ helmet, leftContainer, children }) => (
  <div>
    <Helmet title={helmet} />
    <div className={styles.wd20 + ' ' + styles.align_left}>{leftContainer}</div>
    <div className={styles.wd80}>{children}</div>
  </div>
);

export default PageContainer;
