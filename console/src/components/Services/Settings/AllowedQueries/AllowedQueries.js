import React from 'react';

import AllowedQueriesNotes from './AllowedQueriesNotes';
import AddAllowedQuery from './AddAllowedQuery';
import AllowedQueriesList from './AllowedQueriesList';

import styles from './AllowedQueries.scss';

import { loadAllowedQueries } from '../Actions';

class AllowedQueries extends React.Component {
  constructor(props) {
    super(props);

    const { dispatch } = this.props;

    dispatch(loadAllowedQueries());
  }

  render() {
    const { dispatch, allowedQueries } = this.props;

    return (
      <div
        className={`${styles.clear_fix} ${styles.padd_left} ${
          styles.padd_top
        } ${styles.metadata_wrapper} container-fluid`}
      >
        <div className={styles.subHeader}>
          <h2 className={`${styles.heading_text} ${styles.remove_pad_bottom}`}>
            Allowed Queries
          </h2>
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
    allowedQueries: state.metadata.allowedQueries,
  };
};

const allowedQueriesConnector = connect =>
  connect(mapStateToProps)(AllowedQueries);

export default allowedQueriesConnector;
