import React from 'react';
import FilterQuery from '../../../../Common/FilterQuery/FilterQuery';
import { FilterRenderProp } from '../../../../Common/FilterQuery/types';
import { stInvocationLogsTable } from '../utils';
import { makeOrderBy } from '../../../../Common/utils/v1QueryUtils';
import { Dispatch } from '../../../../../types';
import InvocationLogsTable from '../../Common/Components/InvocationLogsTable';

type Props = {
  dispatch: Dispatch;
};

const InvocationLogs: React.FC<Props> = props => {
  const { dispatch } = props;

  const renderRows: FilterRenderProp = (
    rows,
    count,
    filterState,
    setFilterState,
    runQuery
  ) => (
    <InvocationLogsTable
      rows={rows}
      filterState={filterState}
      count={count}
      setFilterState={setFilterState}
      runQuery={runQuery}
      columns={['id', 'status', 'event_id', 'created_at']}
      identifier="adhoc-events-logs"
      dispatch={dispatch}
    />
  );

  return (
    <FilterQuery
      dispatch={dispatch}
      table={stInvocationLogsTable}
      render={renderRows}
      relationships={['scheduled_event']}
      presets={{
        sorts: [makeOrderBy('created_at', 'desc')],
        filters: [],
      }}
      triggerOp="invocation"
      triggerType="scheduled"
    />
  );
};

export default InvocationLogs;
