import { SupportedDriver } from '@hasura/shared/types';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { getDriverPrefix } from '@hasura/metadata/helpers';

export type InvokeManualTriggerArgs = {
  name: string;
  source: string;
  payload: Record<string, any>;
};

export type InvokeManualTriggerResult = {
  event_id: string;
};

const useInvokeManualTrigger = () => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return (sourceKind: SupportedDriver, args: InvokeManualTriggerArgs) => {
    const prefix = getDriverPrefix(sourceKind);
    const options = {
      method: 'POST',
      body: JSON.stringify({
        type: `${prefix}_invoke_event_trigger`,
        args,
      }),
    };

    return fetchJson<InvokeManualTriggerResult>(endpoints.metadata, options);
  };
};

export default useInvokeManualTrigger;
