import React from 'react';
import FilterQuery from '../../../../Common/FilterQuery/FilterQuery';
import {
  FilterRenderProp,
  makeValueFilter,
} from '../../../../Common/FilterQuery/types';
import { stEventsTable } from '../utils';
import { ScheduledTrigger } from '../../types';
import { Dispatch } from '../../../../../types';
import EventsTable from '../../Common/Components/EventsTable';
import { makeOrderBy } from '../../../../Common/utils/v1QueryUtils';

type Props = {
  currentTrigger?: ScheduledTrigger;
  dispatch: Dispatch;
};

const PendingEvents: React.FC<Props> = props => {
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
          makeValueFilter('status', '$eq', 'scheduled'),
        ],
        sorts: [makeOrderBy('scheduled_time', 'asc')],
      }}
      relationships={['cron_event_logs']}
    />
  );
};

export default PendingEvents;
