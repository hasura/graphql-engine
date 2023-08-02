import React from 'react';
import { z } from 'zod';
import {
  CopyableInputField,
  useConsoleForm,
} from '../../../../../new-components/Form';
import { buildAgentPath, buildDockerCommand } from '../utils';
import { AgentFormValues } from './useAgentForm';

const schema = z.object({ command: z.string(), agentPath: z.string() });

export const useDockerCommandForm = ({
  containerName,
  port,
  path,
  protocol,
}: AgentFormValues) => {
  const { Form, methods } = useConsoleForm({
    schema,
    options: {
      defaultValues: {
        command: buildDockerCommand(containerName, port),
        agentPath: buildAgentPath('docker.host.internal', 8081, 'http'),
      },
    },
  });

  React.useEffect(() => {
    methods.setValue('command', buildDockerCommand(containerName, port));
  }, [containerName, port]);

  React.useEffect(() => {
    methods.setValue('agentPath', buildAgentPath(path, port, protocol));
  }, [path, port, protocol]);

  const DockerCommandForm = () => (
    <Form onSubmit={data => {}}>
      <CopyableInputField
        inputClassName="text-xs"
        label="Install Command"
        tooltip="Use this Docker command to install the Hasura Data Connector Agent"
        disabled
        learnMoreLink="https://hasura.io/blog/hasura-graphql-data-connectors/"
        name="command"
      />
    </Form>
  );

  return {
    DockerCommandForm,
  };
};
