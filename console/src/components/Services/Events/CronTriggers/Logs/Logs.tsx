import React from 'react';
import FilterQuery from '../../../../Common/FilterQuery/FilterQuery';
import {
  FilterRenderProp,
  makeValueFilter,
  makeRelationshipFilter,
} from '../../../../Common/FilterQuery/types';
import { stInvocationLogsTable } from '../utils';
import { makeOrderBy } from '../../../../Common/utils/v1QueryUtils';
import { Dispatch } from '../../../../../types';
import InvocationLogsTable from '../../Common/Components/InvocationLogsTable';
import { ScheduledTrigger } from '../../types';

type Props = {
  currentTrigger?: ScheduledTrigger;
  dispatch: Dispatch;
};

const InvocationLogs: React.FC<Props> = props => {
  const { dispatch, currentTrigger } = props;

  const triggerName = currentTrigger?.name ?? '';

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
      identifier={triggerName}
      dispatch={dispatch}
    />
  );

  return (
    <FilterQuery
      table={stInvocationLogsTable}
      dispatch={dispatch}
      render={renderRows}
      relationships={['cron_event']}
      presets={{
        sorts: [makeOrderBy('created_at', 'desc')],
        filters: [
          makeRelationshipFilter(
            'cron_event',
            makeValueFilter('trigger_name', '$eq', triggerName)
          ),
        ],
      }}
      triggerName={triggerName}
      triggerOp="invocation"
      triggerType="cron"
    />
  );
};

export default InvocationLogs;
