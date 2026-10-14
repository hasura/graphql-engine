import React from 'react';
import { z } from 'zod';
import { CopyableInputField, useConsoleForm } from '@hasura/shared/ui';
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

  // `setValue` is referentially stable across renders (react-hook-form), so
  // depending on it (rather than the whole `methods` object) keeps these
  // field-sync effects from re-running on every render.
  const { setValue } = methods;

  React.useEffect(() => {
    setValue('command', buildDockerCommand(containerName, port));
  }, [setValue, containerName, port]);

  React.useEffect(() => {
    setValue('agentPath', buildAgentPath(path, port, protocol));
  }, [setValue, path, port, protocol]);

  const DockerCommandForm = () => (
    <Form
      onSubmit={() => {
        /* noop */
      }}
    >
      <CopyableInputField
        label="Install Command"
        tooltip="Use this Docker command to install the Hasura Data Connector Agent"
        learnMoreLink="https://hasura.io/blog/hasura-graphql-data-connectors/"
        name="command"
        noErrorPlaceholder
        fieldProps={{
          className: 'text-xs',
          disabled: true,
        }}
      />
    </Form>
  );

  return {
    DockerCommandForm,
  };
};
