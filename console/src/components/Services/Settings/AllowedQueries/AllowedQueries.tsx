import React from 'react';

import AllowedQueriesNotes from './AllowedQueriesNotes';
import AddAllowedQuery from './AddAllowedQuery';
import AllowedQueriesList from './AllowedQueriesList';

import styles from './AllowedQueries.scss';
import { getAllowedQueries } from '../../../../metadata/selector';
import { Dispatch, ReduxState } from '../../../../types';
import { mapDispatchToPropsEmpty } from '../../../Common/utils/reactUtils';
import { AllowedQueriesCollection } from '../../../../metadata/reducer';

interface Props {
  dispatch: Dispatch;
  allowedQueries: AllowedQueriesCollection[];
}

const AllowedQueries: React.FC<Props> = props => {
  const { dispatch, allowedQueries } = props;

  return (
    <div
      className={`${styles.clear_fix} ${styles.padd_left} ${styles.padd_top} ${styles.metadata_wrapper} container-fluid`}
    >
      <div className={styles.subHeader}>
        <h2 className={styles.headerText}>Allow List</h2>
        <div className={`${styles.add_mar_top} ${styles.wd60}`}>
          <AllowedQueriesNotes />
          <hr className="my-md" />
          <AddAllowedQuery
            dispatch={dispatch}
            allowedQueries={allowedQueries}
          />
          <hr className="my-md" />
          <AllowedQueriesList
            dispatch={dispatch}
            allowedQueries={allowedQueries}
          />
        </div>
      </div>
    </div>
  );
};

const mapStateToProps = (state: ReduxState) => {
  return {
    allowedQueries: getAllowedQueries(state),
  };
};

const allowedQueriesConnector = (connect: any) =>
  connect(mapStateToProps, mapDispatchToPropsEmpty)(AllowedQueries);

export default allowedQueriesConnector;
