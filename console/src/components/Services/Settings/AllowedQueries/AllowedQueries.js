import React from 'react';

import AllowedQueriesNotes from './AllowedQueriesNotes';
import AddAllowedQuery from './AddAllowedQuery';
import AllowedQueriesList from './AllowedQueriesList';

import styles from './AllowedQueries.scss';
import { getAllowedQueries } from '../../../../metadata/selector';

class AllowedQueries extends React.Component {
  render() {
    const { dispatch, allowedQueries } = this.props;

    return (
      <div
        className={`${styles.clear_fix} ${styles.padd_left} ${styles.padd_top} ${styles.metadata_wrapper} container-fluid`}
      >
        <div className={styles.subHeader}>
          <h2 className={styles.headerText}>Allow List</h2>
          <div className={styles.add_mar_top + ' ' + styles.wd60}>
            <AllowedQueriesNotes />
            <hr />
            <AddAllowedQuery
              dispatch={dispatch}
              isEmptyList={allowedQueries.length === 0}
            />
            <hr />
            <AllowedQueriesList
              dispatch={dispatch}
              allowedQueries={allowedQueries}
            />
          </div>
        </div>
      </div>
    );
  }
}

const mapStateToProps = state => {
  return {
    allowedQueries: getAllowedQueries(state),
  };
};

const allowedQueriesConnector = connect =>
  connect(mapStateToProps)(AllowedQueries);

export default allowedQueriesConnector;
