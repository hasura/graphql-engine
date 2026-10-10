import React from 'react';
import TableHeader from './TableCommon/TableHeader';
import InvocationLogsTable from '../../components/InvocationLogsTable';
import { useAppContext } from '@hasura/shared/context';
import { useEventTriggerDetailContext } from '../context';
import { usePagination } from '@hasura/shared/hooks';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import useGetEventInvocations from '../hooks/useGetEventInvocations';

const ETInvocationLogs: React.FC = () => {
  const { readOnlyMode } = useAppContext();
  const { eventTrigger, currentSource } = useEventTriggerDetailContext();
  const { paginationState, setPaginationState } = usePagination();
  const { data: rows } = useGetEventInvocations({
    name: eventTrigger.name,
    source: currentSource,
    limit: paginationState.limit,
    offset: paginationState.offset,
  });

  return (
    <React.Fragment>
      <TableHeader
        count={null}
        triggerName={eventTrigger.name}
        tabName="logs"
        readOnlyMode={readOnlyMode}
      />
      <br />
      <Analytics name="EventTriggerInvocations" {...REDACT_EVERYTHING}>
        <div className="mt-6" data-test="event-filter-table">
          <div className="bootstrap-jail">
            <InvocationLogsTable
              rows={rows ?? []}
              columns={['id', 'redeliver', 'status', 'event_id', 'created_at']}
              paginationState={paginationState}
              setPaginationState={setPaginationState}
              source={currentSource}
            />
          </div>
        </div>
      </Analytics>
    </React.Fragment>
  );
};

export default ETInvocationLogs;
