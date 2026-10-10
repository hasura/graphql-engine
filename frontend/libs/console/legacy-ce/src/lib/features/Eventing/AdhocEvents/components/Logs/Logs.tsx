import InvocationLogsTable from '../../../components/InvocationLogsTable';
import { usePagination } from '@hasura/shared/hooks';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import useGetScheduledEventInvocations from '../../../CronTriggers/hooks/useGetScheduledEventInvocations';

const InvocationLogs: React.FC = () => {
  const { paginationState, setPaginationState } = usePagination();
  const { data: rows } = useGetScheduledEventInvocations({
    type: 'one_off',
    limit: paginationState.limit,
    offset: paginationState.offset,
    get_rows_count: false,
  });

  return (
    <Analytics name="AdhocEventLogs" {...REDACT_EVERYTHING}>
      <div className="mt-6" data-test="event-filter-table">
        <InvocationLogsTable
          rows={rows ?? []}
          columns={['id', 'status', 'event_id', 'created_at']}
          paginationState={paginationState}
          setPaginationState={setPaginationState}
        />
      </div>
    </Analytics>
  );
};

export default InvocationLogs;
