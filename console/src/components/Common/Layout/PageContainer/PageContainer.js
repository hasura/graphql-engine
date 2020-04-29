import React from 'react';
import Helmet from 'react-helmet';

import { Box } from '../../../UIKit/atoms';
import styles from '../../Common.scss';

const PageContainer = ({ helmet, leftContainer, children }) => (
  <div>
    <Helmet title={helmet} />
    <Box width="20%" className={styles.align_left}>
      {leftContainer}
    </Box>
    <Box width="80%" display="inline-block">
      {children}
    </Box>
  </div>
);

export default PageContainer;
