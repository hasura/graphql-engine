import React from 'react';
import TableHeader from './TableCommon/TableHeader';
import EventsTable from '../../components/EventsTable';
import { useAppContext } from '@hasura/shared/context';
import { useEventTriggerDetailContext } from '../context';
import { usePagination } from '@hasura/shared/hooks';
import useGetEventLogs from '../hooks/useGetEventLogs';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import EventsSubTable from './EventsSubTable';
import { EventLog } from '@hasura/shared/types';

const ETProcessedEvents: React.FC = () => {
  const { readOnlyMode } = useAppContext();
  const { eventTrigger, currentSource } = useEventTriggerDetailContext();
  const { paginationState, setPaginationState } = usePagination();
  const { data: rows } = useGetEventLogs({
    name: eventTrigger.name,
    source: currentSource,
    limit: paginationState.limit,
    offset: paginationState.offset,
    status: 'processed',
  });

  return (
    <React.Fragment>
      <TableHeader
        triggerName={eventTrigger.name}
        tabName="processed"
        readOnlyMode={readOnlyMode}
      />
      <br />

      <Analytics name="ProcessedEventTriggers" {...REDACT_EVERYTHING}>
        <div className="mt-6" data-test="event-filter-table">
          <EventsTable
            rows={rows ?? []}
            paginationState={paginationState}
            setPaginationState={setPaginationState}
            columns={['id', 'delivered', 'created_at', 'tries']}
            renderExpand={(row: EventLog) => (
              <EventsSubTable
                eventId={row.id}
                webhookConf={
                  eventTrigger.webhook_from_env || eventTrigger.webhook
                }
              />
            )}
          />
        </div>
      </Analytics>
    </React.Fragment>
  );
};

export default ETProcessedEvents;
