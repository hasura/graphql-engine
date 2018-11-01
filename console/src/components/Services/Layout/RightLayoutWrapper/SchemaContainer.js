import React from 'react';

const SchemaContainer = ({ children }) => {
  const styles = require('./SchemaContainer.scss');
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

const schemaContainerConnector = connect =>
  connect(mapStateToProps)(SchemaContainer);

export default schemaContainerConnector;
