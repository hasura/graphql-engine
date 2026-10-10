import InvocationLogsTable from '../../../components/InvocationLogsTable';
import { usePagination } from '@hasura/shared/hooks';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import useGetScheduledEventInvocations from '../../hooks/useGetScheduledEventInvocations';

type Props = {
  triggerName: string;
};

const InvocationLogs: React.FC<Props> = ({ triggerName }) => {
  const { paginationState, setPaginationState } = usePagination();
  const { data: rows } = useGetScheduledEventInvocations({
    trigger_name: triggerName,
    type: 'cron',
    limit: paginationState.limit,
    offset: paginationState.offset,
    get_rows_count: false,
  });

  return (
    <Analytics name="ScheduledTriggerLogs" {...REDACT_EVERYTHING}>
      <div className="mt-6" data-test="event-filter-table">
        <div className="bootstrap-jail">
          <InvocationLogsTable
            rows={rows ?? []}
            columns={['id', 'status', 'event_id', 'created_at']}
            paginationState={paginationState}
            setPaginationState={setPaginationState}
          />
        </div>
      </div>
    </Analytics>
  );
};

export default InvocationLogs;
