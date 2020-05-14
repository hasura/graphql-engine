import React from 'react';
import styles from './RightContainer.scss';
import { MapReduxToProps, ComponentReduxConnector } from '../../../../types';

const RightContainer: React.FC<React.ComponentProps<'div'>> = ({
  children,
}) => {
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

const mapStateToProps: MapReduxToProps = state => {
  return {
    schema: state.tables.allSchemas,
  };
};

const rightContainerConnector: ComponentReduxConnector = connect =>
  connect(mapStateToProps)(RightContainer);

export default rightContainerConnector;
