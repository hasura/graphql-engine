import React from 'react';
import styles from './RightContainer.module.scss';

const RightContainer: React.FC<{
  style?: React.CSSProperties;
}> = ({ children, style }) => {
  return (
    <div className="container-fluid">
      <div className="row">
        <div
          className={`${styles.main} ${styles.padd_left_remove} ${styles.padd_top}`}
          style={style}
        >
          <div className={`${styles.rightBar}`}>{children}</div>
        </div>
      </div>
    </div>
  );
};

export default RightContainer;
