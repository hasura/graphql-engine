import React from 'react';
import useGetScheduledEventInvocations from '../../hooks/useGetScheduledEventInvocations';
import EventSubTable from './EventSubTable';
import { sanitiseRow } from '../../../utils';
import { IndicatorCard, SkeletonList, Text } from '@hasura/shared/ui';
import { Em } from '@radix-ui/themes';
import { ScheduledEventInvocation } from '@hasura/shared/types';

type Props = {
  webhookConf?: string;
  comment?: string;
  eventId: string;
} & (
  | {
      type: 'cron';
      triggerName: string;
    }
  | {
      type: 'one_off';
    }
);

const CronEventSubTable: React.FC<Props> = (props) => {
  const {
    data: invocations,
    isLoading,
    error,
  } = useGetScheduledEventInvocations({
    event_id: props.eventId,
    ...(props.type === 'cron'
      ? {
          type: 'cron',
          trigger_name: props.triggerName,
        }
      : {
          type: 'one_off',
        }),
  });

  if (isLoading) {
    return (
      <div className="p-4">
        <div className="pb-2">
          <Text weight="medium">Recent Invocations:</Text>
        </div>
        <SkeletonList count={5} />
      </div>
    );
  }

  if (error) {
    return (
      <IndicatorCard status="negative" showIcon>
        Error occurred in fetching information about recent invocations.
      </IndicatorCard>
    );
  }

  if (!invocations?.length) {
    return (
      <Text as="p" align="center">
        <Em>There has not been any triggered cron invocation yet</Em>
      </Text>
    );
  }

  const invocationRows =
    invocations?.map((r: any, i: number) => {
      const newRow = {} as ScheduledEventInvocation;
      // Insert cells corresponding to all rows
      Object.keys(r).forEach((col) => {
        newRow[col] = sanitiseRow(col, r);
      });

      return newRow;
    }) ?? [];

  return (
    <EventSubTable
      rows={invocationRows}
      comment={props.comment}
      webhookConf={props.webhookConf}
    />
  );
};

export default CronEventSubTable;
