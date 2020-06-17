import React from 'react';
import { Connect } from 'react-redux';
import styles from './RightContainer.scss';

const RightContainer: React.FC = ({ children }) => {
  return (
    <div className={`${styles.container} container-fluid`}>
      <div className="row">
        <div
          className={`${styles.main} ${styles.padd_left_remove} ${styles.padd_top}`}
        >
          <div className={`${styles.rightBar} `}>{children}</div>
        </div>
      </div>
    </div>
  );
};

const rightContainerConnector = (connect: Connect) => connect()(RightContainer);
export { RightContainer };
export default rightContainerConnector;
