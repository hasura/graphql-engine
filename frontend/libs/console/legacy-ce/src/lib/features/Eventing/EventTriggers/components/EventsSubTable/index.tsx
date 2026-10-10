import React from 'react';
import { useEventTriggerDetailContext } from '../../context';
import useGetEventsById from '../../hooks/useGetEventsById';
import SubTable from './SubTable';
import { SkeletonList, Text } from '@hasura/shared/ui';
import { sanitiseRow } from '../../../utils';

interface Props {
  eventId: string;
  webhookConf?: string;
}

const EventsSubTable: React.FC<Props> = ({ eventId, webhookConf }) => {
  const { currentSource } = useEventTriggerDetailContext();
  const {
    data: inv,
    error,
    isLoading,
  } = useGetEventsById({
    event_id: eventId,
    source: currentSource,
  });

  if (!inv && isLoading) {
    return (
      <div className="p-4">
        <div className="pb-1">
          <Text weight="medium">Recent Invocations:</Text>
        </div>
        <SkeletonList count={5} />
      </div>
    );
  }

  if (error) {
    return (
      <Text>
        <br />
        Error occurred in fetching information about recent invocations.
        <br />
      </Text>
    );
  }

  const invocationRows =
    inv?.map((r: any, i: number) => {
      const newRow: Record<string, unknown> = {};
      // Insert cells corresponding to all rows
      Object.keys(r).forEach((col) => {
        newRow[col] = sanitiseRow(col, r);
      });

      return newRow;
    }) ?? [];

  return <SubTable rows={invocationRows} webhookConf={webhookConf} />;
};

export default EventsSubTable;
