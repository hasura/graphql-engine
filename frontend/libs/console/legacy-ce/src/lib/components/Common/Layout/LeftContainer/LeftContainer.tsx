import React from 'react';
import styles from '../../TableCommon/Table.module.scss';
import { SIDEBAR_ID } from '../../../../features/DataSidebar/constants';

const LeftContainer: React.FC = ({ children }) => {
  return (
    <div
      id={SIDEBAR_ID}
      className={`${styles.pageSidebar} col-xs-12 ${styles.padd_remove}`}
    >
      {children}
    </div>
  );
};

export default LeftContainer;
