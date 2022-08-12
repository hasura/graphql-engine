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
import {
  getConfirmation,
  convertDateTimeToLocale,
} from '../../../../Common/utils/jsUtils';

type Props = {
  dispatch: Dispatch;
};

const PendingEvents: React.FC<Props> = props => {
  const { dispatch } = props;

  const onCancelOneOffScheduledEvent = (
    id: string,
    scheduledAt: string | Date | number,
    onSuccess: () => void
  ) => {
    const localeTime = convertDateTimeToLocale(scheduledAt);
    const shouldCancelEvent = getConfirmation(
      `This will delete the one-off event "${id}" scheduled for "${localeTime}"`
    );
    if (shouldCancelEvent) {
      dispatch(cancelEvent('one_off', id, onSuccess));
    }
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
        'actions',
        'id',
        'status',
        'scheduled_time',
        'created_at',
        'tries',
      ]}
      identifier="adhoc-events-processed"
      onCancelEvent={onCancelOneOffScheduledEvent}
      triggerType="one_off"
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
      triggerOp="pending"
      triggerType="scheduled"
    />
  );
};

export default PendingEvents;
