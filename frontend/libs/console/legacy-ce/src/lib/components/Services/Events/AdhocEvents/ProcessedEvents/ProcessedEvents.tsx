import React from 'react';
import FilterQuery from '../../../../Common/FilterQuery/FilterQuery';
import {
  FilterRenderProp,
  makeValueFilter,
} from '../../../../Common/FilterQuery/types';
import { adhocEventsTable } from '../utils';
import EventsTable from '../../Common/Components/EventsTable';
import { makeOrderBy } from '../../../../Common/utils/v1QueryUtils';
import { Dispatch } from '../../../../../types';

type Props = {
  dispatch: Dispatch;
};

const ProcessedEvents: React.FC<Props> = props => {
  const { dispatch } = props;

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
      identifier="adhoc-events-processed"
      triggerType="one_off"
    />
  );

  return (
    <FilterQuery
      table={adhocEventsTable}
      dispatch={dispatch}
      render={renderRows}
      presets={{
        filters: [makeValueFilter('status', '$ne', 'scheduled')],
        sorts: [makeOrderBy('scheduled_time', 'desc')],
      }}
      relationships={['scheduled_event_logs']}
      triggerType="scheduled"
      triggerOp="processed"
    />
  );
};

export default ProcessedEvents;
