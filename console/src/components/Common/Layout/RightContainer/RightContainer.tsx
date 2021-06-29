import React from 'react';
import styles from './RightContainer.scss';

const RightContainer: React.FC = ({ children }) => {
  return (
    <div className="container-fluid">
      <div className="row">
        <div
          className={`${styles.main} ${styles.padd_left_remove} ${styles.padd_top}`}
        >
          <div className={`${styles.rightBar}`}>{children}</div>
        </div>
      </div>
    </div>
  );
};

export default RightContainer;
