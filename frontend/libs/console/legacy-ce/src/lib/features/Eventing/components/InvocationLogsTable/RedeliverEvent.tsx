import React from 'react';
import { Em, Flex } from '@radix-ui/themes';
import useGetEventsById from '../../EventTriggers/hooks/useGetEventsById';
import {
  SkeletonList,
  IndicatorCard,
  Text,
  JsonCodeBlock,
} from '@hasura/shared/ui';
import RedeliverEventLogsTable from './RedeliverEventLogsTable';
import { QualifiedDataSource } from '@hasura/shared/types';

type Props = {
  eventId: string;
  source: QualifiedDataSource;
};

const RedeliverEvent: React.FC<Props> = ({ eventId, source }) => {
  const {
    data: logs,
    isLoading,
    error,
  } = useGetEventsById(
    {
      event_id: eventId,
      source,
    },
    { refetchInterval: 2000 },
  );

  const [expandedRowIndex, setExpandedRowIndex] = React.useState<number | null>(
    null,
  );

  const invocationColumns = ['http_status', 'id', 'created_at'];

  if (!logs?.length && !isLoading) {
    return <SkeletonList count={5} />;
  }

  if (error) {
    return (
      <IndicatorCard status="negative" showIcon>
        There was an error in fetching the details of the recent invocations.
      </IndicatorCard>
    );
  }

  const latestLog = logs?.[0];

  return (
    <div className="content-fluid">
      <div>
        <Flex className="w-full" gap="2">
          <div className="w-1/2">
            <Text> Request </Text>
            <JsonCodeBlock
              className="mt-2"
              value={latestLog?.request ?? ''}
              hideCopyButton
              scrollable
            />
          </div>
          <div className="w-1/2">
            <Text>Latest Invocation Response</Text>
            <JsonCodeBlock
              className="mt-2"
              value={error || latestLog?.response}
              hideCopyButton
              scrollable
            />
          </div>
        </Flex>
        <div className="mt-4">
          <Text>
            <Em>Recent Invocations</Em>
          </Text>
          <RedeliverEventLogsTable
            className="mt-4"
            logs={logs ?? []}
            invocationColumns={invocationColumns}
            expandedRowIndex={expandedRowIndex}
            setExpandedRowIndex={setExpandedRowIndex}
          />
        </div>
      </div>
    </div>
  );
};

export default RedeliverEvent;
