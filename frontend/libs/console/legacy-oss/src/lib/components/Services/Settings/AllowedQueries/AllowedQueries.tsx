import React from 'react';

import AllowedQueriesNotes from './AllowedQueriesNotes';
import AddAllowedQuery from './AddAllowedQuery';
import AllowedQueriesList from './AllowedQueriesList';

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
    <div className="clear-both pl-md pt-md mb-md">
      <div>
        <h2 className="text-xl font-bold">Allow List</h2>
        <div className="mt-md w-1/2">
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
