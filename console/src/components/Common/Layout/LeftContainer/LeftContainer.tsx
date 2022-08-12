import React from 'react';
import styles from '../../TableCommon/Table.scss';

const LeftContainer: React.FC = ({ children }) => {
  return (
    <div className={`${styles.pageSidebar} col-xs-12 ${styles.padd_remove}`}>
      <div>{children}</div>
    </div>
  );
};

export default LeftContainer;
