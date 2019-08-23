import React from 'react';

const RightContainer = ({ children }) => {
  const styles = require('./RightContainer.scss');

  return (
    <div className={styles.container + ' container-fluid'}>
      <div className="row">
        <div
          className={
            styles.main + ' ' + styles.padd_left_remove + ' ' + styles.padd_top
          }
        >
          <div className={styles.rightBar + ' '}>
            {children && React.cloneElement(children)}
          </div>
        </div>
      </div>
    </div>
  );
};

const mapStateToProps = state => {
  return {
    schema: state.tables.allSchemas,
  };
};

const rightContainerConnector = connect =>
  connect(mapStateToProps)(RightContainer);

export default rightContainerConnector;
