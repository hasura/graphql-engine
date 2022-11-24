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
import { cancelEvent } from '../../ServerIO';
import {
  getConfirmation,
  convertDateTimeToLocale,
} from '../../../../Common/utils/jsUtils';

type Props = {
  currentTrigger?: ScheduledTrigger;
  dispatch: Dispatch;
};

const PendingEvents: React.FC<Props> = props => {
  const { dispatch, currentTrigger } = props;
  const triggerName = currentTrigger ? currentTrigger.name : '';

  const onCancelCronTrigger = (
    id: string,
    scheduledAt: string | Date | number,
    onSuccess: () => void
  ) => {
    const localeTime = convertDateTimeToLocale(scheduledAt);
    const shouldCancelEvent = getConfirmation(
      `This will delete the "${id}" of cron trigger "${triggerName}" scheduled for "${localeTime}"`
    );
    if (shouldCancelEvent) {
      dispatch(cancelEvent('cron', id, onSuccess));
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
      identifier={triggerName}
      onCancelEvent={onCancelCronTrigger}
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
          makeValueFilter('status', '$eq', 'scheduled'),
        ],
        sorts: [makeOrderBy('scheduled_time', 'asc')],
      }}
      relationships={['cron_event_logs']}
      triggerName={triggerName}
      triggerType="cron"
      triggerOp="pending"
    />
  );
};

export default PendingEvents;
