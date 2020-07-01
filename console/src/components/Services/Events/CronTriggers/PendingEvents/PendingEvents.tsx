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
import Endpoints from '../../../../../Endpoints';
import requestAction from '../../../../../utils/requestAction';
import {
  showSuccessNotification,
  showErrorNotification,
} from '../../../Common/Notification';

type Props = {
  currentTrigger?: ScheduledTrigger;
  dispatch: Dispatch;
};

const PendingEvents: React.FC<Props> = props => {
  const { dispatch, currentTrigger } = props;
  const triggerName = currentTrigger ? currentTrigger.name : '';

  const onCancelCronTrigger = (id: string, onSuccess: () => void) => {
    const url = Endpoints.query;
    const payload = {
      type: 'delete',
      args: {
        table: { name: 'hdb_cron_events', schema: 'hdb_catalog' },
        where: {
          id: { $eq: id },
        },
      },
    };
    const options = {
      method: 'POST',
      body: JSON.stringify(payload),
    };
    const successText = 'Successfully deleted cron event';
    const errorText = 'Error in cancelling event';

    dispatch(requestAction(url, options, successText, errorText, true, true))
      .then(() => {
        dispatch(showSuccessNotification(successText));
        onSuccess();
      })
      .catch(err => {
        dispatch(showErrorNotification(errorText, err.message, err));
      });
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
      identifier={triggerName}
      onCancelEvent={onCancelCronTrigger}
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
