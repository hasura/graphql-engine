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
import { cancelEvent } from '../../ServerIO';

type Props = {
  dispatch: Dispatch;
};

const PendingEvents: React.FC<Props> = props => {
  const { dispatch } = props;

  const onCancelOneOffScheduledEvent = (id: string, onSuccess: () => void) => {
    dispatch(
      cancelEvent('one-off scheduled', 'hdb_scheduled_events', id, onSuccess)
    );
  };

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
      columns={[
        'id',
        'status',
        'scheduled_time',
        'created_at',
        'tries',
        'actions',
      ]}
      identifier="adhoc-events-processed"
      onCancelEvent={onCancelOneOffScheduledEvent}
    />
  );

  return (
    <FilterQuery
      table={adhocEventsTable}
      dispatch={dispatch}
      render={renderRows}
      presets={{
        filters: [makeValueFilter('status', '$eq', 'scheduled')],
        sorts: [makeOrderBy('scheduled_time', 'asc')],
      }}
      relationships={['scheduled_event_logs']}
    />
  );
};

export default PendingEvents;
