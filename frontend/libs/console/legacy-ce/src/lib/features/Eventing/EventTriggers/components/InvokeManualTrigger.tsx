import { useEffect, useState } from 'react';
import { Flex } from '@radix-ui/themes';
import { Dialog, hasuraToast, JsonCodeBlock, Spinner } from '@hasura/shared/ui';
import useInvokeManualTrigger, {
  InvokeManualTriggerArgs,
} from '../hooks/useInvokeManualTrigger';
import type { SupportedDriver } from '@hasura/shared/types';
import useGetEventsById from '../hooks/useGetEventsById';
import { getErrorMessage } from '@hasura/shared/utils';

/* This component accepts for following props
 *  1) Trigger name
 *  2) Trigger object
 * It creates a manual trigger and watches the response of the same.
 * */

/*
 * Invokes the event based on the arg object and name and shows the action to the user
 *  - While the request is being made - Invoking
 *  - Once the request is completed
 *    - Success - Track the status
 *    - Failure - Notify user
 * */

type Props = InvokeManualTriggerArgs & {
  sourceKind: SupportedDriver;
  onClose: () => void;
};

const InvokeManualTrigger = ({ onClose, sourceKind, ...invokeArgs }: Props) => {
  const invokeManualTrigger = useInvokeManualTrigger();
  const [isModalOpen, setIsModalOpen] = useState(false);
  const [isCreatingManualTrigger, setIsCreatingManualTrigger] = useState(true);
  const [eventId, setEventId] = useState<string | null>(null);
  const {
    data: invocations,
    isLoading,
    error,
  } = useGetEventsById(
    {
      source: {
        name: invokeArgs.source,
        kind: sourceKind,
      },
      event_id: eventId!,
    },
    {
      enabled: Boolean(eventId),
      refetchInterval: 3000,
    },
  );

  const onModalClose = () => {
    setIsModalOpen(false);
    onClose && onClose();
  };

  useEffect(() => {
    invokeManualTrigger(sourceKind, invokeArgs)
      .then((data) => {
        setEventId(data.event_id);
      })
      .catch((err) => {
        hasuraToast({
          type: 'error',
          title: 'Error invoking trigger',
          message: getErrorMessage(err),
        });
      })
      .finally(() => {
        setIsCreatingManualTrigger(false);
      });
  }, []);

  if (isCreatingManualTrigger) {
    return <Spinner />;
  }

  const getEventPayload = () => {
    if (isLoading || !invocations?.length) {
      return <div>Fetching invocation info {eventId}</div>;
    }

    const latestInvocation = invocations[invocations?.length - 1]!;

    return (
      <Flex>
        <div className={`pl-0 w-1/2 mr-2`}>
          <div> Request </div>
          <JsonCodeBlock value={latestInvocation.request} />
        </div>
        <div className={`pl-0 w-1/2`}>
          <div> Latest Invocation Response</div>
          <JsonCodeBlock value={latestInvocation.response} />
        </div>
      </Flex>
    );
  };
  /*
   * If there is no error, get the event info. Otherwise check if the err is an object with `error` as key. Print a default message finally
   * */
  const getEventIdErrorText = (error) => {
    return <span className="text-red-600">{error}</span>;
  };

  const getEventData = () => (
    <div className="pl-0 w-full clear-both pr-0">
      <div className="pl-0 w-full pr-0">
        <div className={`mb-4`}>
          Event ID -{' '}
          {eventId || getEventIdErrorText('Unable to invoke trigger')}
        </div>
        {error ? <div>{getErrorMessage(error)}</div> : getEventPayload()}
      </div>
    </div>
  );

  if (!isModalOpen) return null;

  return (
    <div>
      <Dialog
        onClose={onModalClose}
        title={`Invoking ${invokeArgs.name}`}
        size="xxxl"
      >
        <div className="p-4 ">{getEventData()}</div>
      </Dialog>
    </div>
  );
};

export default InvokeManualTrigger;
