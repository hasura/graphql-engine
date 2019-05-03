import React from 'react';

import WhitelistNotes from './WhitelistNotes';
import AddWhitelistQuery from './AddWhitelistQuery';
import WhitelistQueriesList from './WhitelistQueriesList';

const WhitelistQueries = props => {
  // const { dispatch, supportQueryWhitelist, whitelistQueries } = props;
  const { dispatch, whitelistQueries } = props;

  const styles = require('./WhitelistQueries.scss');

  return (
    <div
      className={`${styles.clear_fix} ${styles.padd_left} ${styles.padd_top} ${
        styles.metadata_wrapper
      } container-fluid`}
    >
      <div className={styles.subHeader}>
        <h2 className={`${styles.heading_text} ${styles.remove_pad_bottom}`}>
          Whitelist Queries
        </h2>
        <div className={styles.add_mar_top + ' ' + styles.wd60}>
          <WhitelistNotes />
          <hr />
          <AddWhitelistQuery dispatch={dispatch} />
          <hr />
          <WhitelistQueriesList
            dispatch={dispatch}
            whitelistQueries={whitelistQueries}
          />
        </div>
      </div>
    </div>
  );
};

const mapStateToProps = state => {
  return {
    supportQueryWhitelist: state.main.supportQueryWhitelist,
    whitelistQueries: state.metadata.whitelistQueries,
  };
};

const whitelistQueriesConnector = connect =>
  connect(mapStateToProps)(WhitelistQueries);

export default whitelistQueriesConnector;
