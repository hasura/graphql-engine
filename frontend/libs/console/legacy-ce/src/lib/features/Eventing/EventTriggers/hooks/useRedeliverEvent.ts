import { useAuthFetchJson } from '@hasura/shared/hooks';
import { SupportedDriver } from '@hasura/shared/types';
import { getDriverPrefix } from '@hasura/metadata/helpers';
import { useAppContext } from '@hasura/shared/context';

export type RedeliverEventArgs = {
  eventId: string;
  sourceKind: SupportedDriver;
};

export const useRedeliverEvent = () => {
  const fetchJson = useAuthFetchJson();
  const { endpoints } = useAppContext();

  return (args: RedeliverEventArgs) => {
    const prefix = getDriverPrefix(args.sourceKind);

    return fetchJson(endpoints.metadata, {
      method: 'POST',
      body: JSON.stringify({
        type: `${prefix}_redeliver_event`,
        args: {
          event_id: args.eventId,
        },
      }),
    });
  };
};
