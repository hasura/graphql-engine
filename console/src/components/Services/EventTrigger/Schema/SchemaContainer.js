import React from 'react';
import Helmet from 'react-helmet';

// import PageContainer from '../PageContainer/PageContainer';

const SchemaContainer = ({ children }) => {
  const styles = require('./SchemaContainer.scss');
  return (
    <div className={styles.container + ' container-fluid'}>
      <Helmet title={'Schema | Data | Hasura'} />
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
