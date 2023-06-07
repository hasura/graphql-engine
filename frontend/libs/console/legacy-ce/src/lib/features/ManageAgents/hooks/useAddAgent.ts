import { useMetadataMigration } from '../../MetadataAPI';
import { useCallback } from 'react';
import { useQueryClient } from 'react-query';
import { hasuraToast } from '../../../new-components/Toasts';

type AddAgentServerResponse = {
  // this comes from the server
  message?: string;
};
type AddAgentConsoleProps = {
  // these are properties added to the response for console use
  error?: Error | null;
  name: string;
  url: string;
  makeToast: () => void;
  status: 'unavailable' | 'error' | 'already-added' | 'added';
};

export type AddAgentResponse = AddAgentServerResponse & AddAgentConsoleProps;

type AddAgentArgs = { name: string; url: string };

const AGENT_UNAVAILABLE_MESSAGE = 'Agent is not available';

const AGENT_ALREADY_EXISTS_MESSAGE = (driverKind: string) =>
  `SourceKind '${driverKind}' already exists.`;

export const useAddAgent = () => {
  const queryClient = useQueryClient();
  const mutation = useMetadataMigration<AddAgentServerResponse, AddAgentArgs>();

  // because this call has 4 statuses that fall across both "success" and "error" with respect to the API HTTP response,
  // we are always "resolving" the promise, and providing our own response object that aligns with the needs of the console
  const doMutation = useCallback(
    (args: AddAgentArgs) =>
      new Promise<AddAgentResponse>(resolve =>
        mutation.mutate(
          {
            query: {
              type: 'dc_add_agent',
              args,
            },
          },
          {
            // moving the toast notifications to the consumer side so this hook can be re-used in other situations
            onSuccess: serverResponse => {
              resolve({
                ...serverResponse,
                ...args,
                makeToast:
                  serverResponse.message === AGENT_UNAVAILABLE_MESSAGE
                    ? agentUnavailableToast
                    : successToast,
                status:
                  serverResponse.message === AGENT_UNAVAILABLE_MESSAGE
                    ? 'unavailable'
                    : 'added',
              });
            },
            onError: error => {
              resolve({
                ...args,
                error,
                makeToast:
                  error?.message === AGENT_ALREADY_EXISTS_MESSAGE(args.name)
                    ? () => agentAlreadyExistsToast(args.name)
                    : () => agentErrorToast(error),
                status:
                  error?.message === AGENT_ALREADY_EXISTS_MESSAGE(args.name)
                    ? 'already-added'
                    : 'error',
                message: error?.message,
              });
            },
          }
        )
      ),
    [mutation]
  );

  const invalidateRelatedQueries = useCallback(() => {
    queryClient.refetchQueries(['agent_list'], {
      exact: true,
    });
    // since the agents show up in the available drivers, we should invalidate this.
    queryClient.invalidateQueries({
      queryKey: ['get_available_drivers'],
    });
  }, [queryClient]);

  const addMultipleAgents = useCallback(
    async (args: AddAgentArgs[]) => {
      const responses: AddAgentResponse[] = [];
      for await (const arg of args) {
        responses.push(await doMutation(arg));
      }
      invalidateRelatedQueries();
      return responses;
    },
    [doMutation, invalidateRelatedQueries]
  );

  const addAgent = useCallback(
    async (args: AddAgentArgs) => {
      const prom = doMutation(args);

      prom.then(data => {
        if (data.status === 'added') {
          invalidateRelatedQueries();
        }
      });

      return prom;
    },
    [doMutation, invalidateRelatedQueries]
  );

  return {
    addAgent,
    addMultipleAgents,
    ...mutation,
  };
};

const successToast = () =>
  hasuraToast({
    title: 'Success',
    type: 'success',
    message: `Data connector agent connected successfully!`,
  });

const agentUnavailableToast = () =>
  hasuraToast({
    title: 'Error',
    type: 'error',
    message: `The Data Connector Service is not reachable at the URL provided. Double check the path/port/protocol and that the service is currently running.`,
  });

const agentAlreadyExistsToast = (name: string) =>
  hasuraToast({
    title: 'Already Exists',
    type: 'error',
    message: `An agent with the name "${name}" already exists. Please choose a unique name.`,
  });

const agentErrorToast = (error: Error | null) =>
  hasuraToast({
    title: 'Error',
    type: 'error',
    message:
      error?.message ??
      `Error adding Data Connector Agent.\n\n${error?.toString()}`,
  });
