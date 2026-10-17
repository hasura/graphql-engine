import React from 'react';
import { useDestructiveConfirm } from '@hasura/shared/ui';
import { usePagination } from '@hasura/shared/hooks';
import EventsTable from '../../../components/EventsTable';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import type { CronTrigger, ScheduledEvent } from '@hasura/shared/types';
import CronEventSubTable from '../CronEventSubTable';
import {
  useGetScheduledEvents,
  useDeleteScheduledEvent,
} from '@hasura/metadata/api';

type Props = {
  currentTrigger: CronTrigger;
};

const PendingEvents: React.FC<Props> = ({ currentTrigger }) => {
  const { paginationState, setPaginationState } = usePagination({
    sorts: [
      {
        column: 'scheduled_time',
        nulls: 'last',
        type: 'asc',
      },
    ],
  });
  const { data: rows } = useGetScheduledEvents({
    trigger_name: currentTrigger.name,
    type: 'cron',
    limit: paginationState.limit,
    offset: paginationState.offset,
    get_rows_count: false,
    status: ['scheduled'],
    order_by: paginationState.sorts,
  });

  const { mutateAsync: cancelEvent } = useDeleteScheduledEvent();
  const destructiveConfirm = useDestructiveConfirm();

  const onCancelCronTrigger = (
    id: string,
    scheduledAt: string | Date | number,
    onSuccess?: () => void,
  ) => {
    destructiveConfirm({
      resourceName: id,
      resourceType: 'scheduled event',
      destroyTerm: 'delete',
      appendTerm: `This cron event of "${currentTrigger.name}" is scheduled for "${scheduledAt}".`,
      onConfirm: () => {
        return cancelEvent({
          event_id: id,
          type: 'cron',
        })
          .then(() => {
            onSuccess?.();
            return true;
          })
          .catch(() => false);
      },
    });
  };

  return (
    <Analytics name="ScheduledTriggerPendingEvents" {...REDACT_EVERYTHING}>
      <div className="mt-6" data-test="event-filter-table">
        <EventsTable
          rows={rows ?? []}
          paginationState={paginationState}
          setPaginationState={setPaginationState}
          onCancelEvent={onCancelCronTrigger}
          columns={[
            'actions',
            'id',
            'status',
            'scheduled_time',
            'created_at',
            'tries',
          ]}
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

export default PendingEvents;
