import React from 'react';

import AllowedQueriesNotes from './AllowedQueriesNotes';
import AddAllowedQuery from './AddAllowedQuery';
import AllowedQueriesList from './AllowedQueriesList';
import { loadAllowedQueries } from '../Actions';
import { Heading, Box } from '../../../UIKit/atoms';
import styles from './AllowedQueries.scss';

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
        className={`${styles.clear_fix} ${styles.padd_left} ${styles.padd_top} ${styles.metadata_wrapper} container-fluid`}
      >
        <div className={styles.subHeader}>
          <Heading as="h2" pb="0px" fontSize="18px">
            Allowed Queries
          </Heading>
          <Box width="60%" mt="20px">
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
          </Box>
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
