import { useAddAgent } from '../../../../ManageAgents/hooks';
import { AddAgentResponse } from '../../../../ManageAgents/hooks/useAddAgent';
import { SuperConnectorDrivers } from '../../../../hasura-metadata-types';

export const agentPaths: Record<SuperConnectorDrivers, string> = {
  snowflake: '/api/v1/snowflake',
  athena: '/api/v1/athena',
  mysql8: '/api/v1/mysql',
  mariadb: '/api/v1/mariadb',
  oracle: '/api/v1/oracle',
};

function ensure<T>(
  argument: T | undefined | null,
  message = 'This value was promised to be there.'
): T {
  if (argument === undefined || argument === null) {
    throw new TypeError(message);
  }

  return argument;
}

export const useAddSuperConnectorAgents = () => {
  const { addMultipleAgents, ...rest } = useAddAgent();

  const addAgents = async (
    superConnectorPath: string,
    selectedAgent: SuperConnectorDrivers
  ) => {
    const args = Object.entries<string>(agentPaths).map(
      ([driverKind, agentPath]) => ({
        name: driverKind,
        url: superConnectorPath + agentPath,
      })
    );

    const responses = await addMultipleAgents(args);

    const selectedAgentResponse = ensure<AddAgentResponse>(
      responses.find(r => r.name === selectedAgent)
    );

    return {
      // while we are going to attempt to add all known super connector agents at the same time,
      // our success criteria is only that the selected agents was added or was already added.
      success:
        selectedAgentResponse.status === 'added' ||
        selectedAgentResponse.status === 'already-added',
      responses: responses,
      makeToast: selectedAgentResponse.makeToast,
    };
  };

  return { addAgents, ...rest };
};
