import React from 'react';
import { connect } from 'react-redux';

const styles = require('./RightContainer.scss');

const RightContainer = ({ children }) => (
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

const mapStateToProps = state => {
  return {
    schema: state.tables.allSchemas,
  };
};

export default connect(mapStateToProps)(RightContainer);
