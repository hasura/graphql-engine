import React from 'react';
import FilterQuery from '../../../../Common/FilterQuery/FilterQuery';
import {
  FilterRenderProp,
  makeValueFilter,
} from '../../../../Common/FilterQuery/types';
import { stEventsTable } from '../utils';
import { ScheduledTrigger } from '../../types';
import EventsTable from '../../Common/Components/EventsTable';
import { makeOrderBy } from '../../../../Common/utils/v1QueryUtils';
import { Dispatch } from '../../../../../types';

type Props = {
  dispatch: Dispatch;
  currentTrigger?: ScheduledTrigger;
};

const ProcessedEvents: React.FC<Props> = props => {
  const { dispatch, currentTrigger } = props;

  const triggerName = currentTrigger ? currentTrigger.name : '';

  const renderRows: FilterRenderProp = (
    rows,
    count,
    filterState,
    setFilterState,
    runQuery
  ) => (
    <EventsTable
      rows={rows}
      count={count}
      filterState={filterState}
      setFilterState={setFilterState}
      runQuery={runQuery}
      columns={['id', 'status', 'scheduled_time', 'created_at', 'tries']}
      identifier={triggerName}
      triggerType="cron"
    />
  );

  return (
    <FilterQuery
      table={stEventsTable}
      dispatch={dispatch}
      render={renderRows}
      presets={{
        filters: [
          makeValueFilter('trigger_name', '$eq', triggerName),
          makeValueFilter('status', '$ne', 'scheduled'),
        ],
        sorts: [makeOrderBy('scheduled_time', 'desc')],
      }}
      relationships={['cron_event_logs']}
      triggerName={triggerName}
      triggerType="cron"
      triggerOp="processed"
    />
  );
};

export default ProcessedEvents;
