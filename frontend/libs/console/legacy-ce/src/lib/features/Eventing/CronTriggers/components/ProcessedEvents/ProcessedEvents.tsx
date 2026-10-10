import React from 'react';
import { usePagination } from '@hasura/shared/hooks';
import useGetScheduledEvents from '../../hooks/useGetScheduledEvents';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import EventsTable from '../../../components/EventsTable';
import CronEventSubTable from '../CronEventSubTable';
import { CronTrigger, ScheduledEvent } from '@hasura/shared/types';

type Props = {
  currentTrigger: CronTrigger;
};

const ProcessedEvents: React.FC<Props> = ({ currentTrigger }) => {
  const { paginationState, setPaginationState } = usePagination({
    sorts: [
      {
        column: 'scheduled_time',
        nulls: 'last',
        type: 'desc',
      },
    ],
  });

  const { data: rows } = useGetScheduledEvents({
    trigger_name: currentTrigger.name,
    type: 'cron',
    limit: paginationState.limit,
    offset: paginationState.offset,
    get_rows_count: false,
    status: ['error', 'delivered', 'dead'],
    order_by: paginationState.sorts,
  });

  return (
    <Analytics name="ScheduledTriggerProcessedEvents" {...REDACT_EVERYTHING}>
      <div className="mt-6" data-test="event-filter-table">
        <EventsTable
          rows={rows ?? []}
          paginationState={paginationState}
          setPaginationState={setPaginationState}
          columns={['id', 'status', 'scheduled_time', 'created_at', 'tries']}
          renderExpand={(row: ScheduledEvent) => {
            return (
              <CronEventSubTable
                eventId={row.id}
                triggerName={currentTrigger.name}
                type="cron"
                comment={currentTrigger.comment}
                webhookConf={currentTrigger.webhook}
              />
            );
          }}
        />
      </div>
    </Analytics>
  );
};

export default ProcessedEvents;
