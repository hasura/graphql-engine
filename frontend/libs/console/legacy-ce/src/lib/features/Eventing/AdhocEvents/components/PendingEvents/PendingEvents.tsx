import { convertDateTimeToLocale, getConfirmation } from '@hasura/shared/utils';
import EventsTable from '../../../components/EventsTable';
import { usePagination } from '@hasura/shared/hooks';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import CronEventSubTable from '../../../CronTriggers/components/CronEventSubTable';
import useDeleteScheduledEvent from '../../../CronTriggers/hooks/useDeleteScheduledEvent';
import useGetScheduledEvents from '../../../CronTriggers/hooks/useGetScheduledEvents';
import { ScheduledEvent } from '@hasura/shared/types';

const PendingEvents: React.FC = () => {
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
    type: 'one_off',
    limit: paginationState.limit,
    offset: paginationState.offset,
    get_rows_count: false,
    status: ['scheduled'],
    order_by: paginationState.sorts,
  });

  const cancelEvent = useDeleteScheduledEvent();

  const onCancelOneOffScheduledEvent = (
    id: string,
    scheduledAt: string | Date | number,
    onSuccess?: () => void,
  ) => {
    const localeTime = convertDateTimeToLocale(scheduledAt);
    const shouldCancelEvent = getConfirmation(
      `This will delete the one-off event "${id}" scheduled for "${localeTime}"`,
    );
    if (shouldCancelEvent) {
      cancelEvent({
        event_id: id,
        type: 'one_off',
      }).then(() => {
        onSuccess?.();
      });
    }
  };

  return (
    <Analytics name="AdhocEventPendingEvents" {...REDACT_EVERYTHING}>
      <div className="mt-6" data-test="event-filter-table">
        <EventsTable
          rows={rows ?? []}
          paginationState={paginationState}
          setPaginationState={setPaginationState}
          onCancelEvent={onCancelOneOffScheduledEvent}
          columns={[
            'actions',
            'id',
            'status',
            'scheduled_time',
            'created_at',
            'tries',
          ]}
          renderExpand={(row: ScheduledEvent) => {
            return <CronEventSubTable eventId={row.id} type="one_off" />;
          }}
        />
      </div>
    </Analytics>
  );
};

export default PendingEvents;
