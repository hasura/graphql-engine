import EventsTable from '../../../components/EventsTable';
import { usePagination } from '@hasura/shared/hooks';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import CronEventSubTable from '../../../CronTriggers/components/CronEventSubTable';
import useGetScheduledEvents from '../../../CronTriggers/hooks/useGetScheduledEvents';
import { ScheduledEvent } from '@hasura/shared/types';

const ProcessedEvents: React.FC = () => {
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
    type: 'one_off',
    limit: paginationState.limit,
    offset: paginationState.offset,
    get_rows_count: false,
    status: ['error', 'delivered', 'dead'],
    order_by: paginationState.sorts,
  });

  return (
    <Analytics name="AdhocProcessedEvents" {...REDACT_EVERYTHING}>
      <div className="mt-6" data-test="event-filter-table">
        <EventsTable
          rows={rows ?? []}
          paginationState={paginationState}
          setPaginationState={setPaginationState}
          columns={['id', 'status', 'scheduled_time', 'created_at', 'tries']}
          renderExpand={(row: ScheduledEvent) => {
            return <CronEventSubTable eventId={row.id} type="one_off" />;
          }}
        />
      </div>
    </Analytics>
  );
};

export default ProcessedEvents;
