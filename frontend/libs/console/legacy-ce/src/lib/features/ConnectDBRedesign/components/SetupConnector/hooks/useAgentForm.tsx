import { MdOutlineTipsAndUpdates } from 'react-icons/md';
import { z } from 'zod';
import { Flex } from '@radix-ui/themes';
import { InputField, Select, Text, useConsoleForm } from '@hasura/shared/ui';

const schema = z.object({
  port: z.coerce.number().min(1).max(65535),
  containerName: z.string().min(1),
  path: z.string().min(1),
  protocol: z.union([z.literal('http'), z.literal('https')]),
});

export type AgentFormValues = z.infer<typeof schema>;

export const useAgentForm = () => {
  const {
    Form,
    methods: { watch, setValue },
  } = useConsoleForm({
    schema,
    options: {
      mode: 'onBlur',
      defaultValues: {
        port: 8081,
        containerName: 'hasura-graphql-data-connector',
        path: 'host.docker.internal',
        protocol: 'http',
      },
    },
  });

  const { port, containerName, path, protocol } = watch();

  const agentPath = `http://${path}:${port}`;

  const AgentForm = () => (
    <Form onSubmit={(data) => {}}>
      <div>
        <Flex className="mb-4" gap="2" align="center">
          <MdOutlineTipsAndUpdates />
          <Text>
            Changing these values will dynamically alter the install command.
          </Text>
        </Flex>
        <Flex direction="row">
          <InputField
            name="path"
            label="Network Path"
            tooltip="This is the network path that Hasura will use to communicate with the Data Connector Service."
            inputTransform={(v) => v.replace(' ', '')}
            description="Protocol &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Host Name / IP Address"
            fieldProps={{
              type: 'text',
              containerClassName: 'rounded-r-none',
              prependLabel: (
                <Select
                  placeholder="Protocol"
                  onChange={(value) => {
                    setValue('protocol', value as 'http' | 'https');
                  }}
                  options={[
                    {
                      value: 'http',
                      label: 'http',
                    },
                    {
                      value: 'https',
                      label: 'https',
                    },
                  ]}
                />
              ),
              placeholder: '127.0.0.1',
            }}
          />
          <style>{`.docker-config-port span.prepend-label { border-radius: 0; }`}</style>
          <div className="w-48 docker-config-port">
            <InputField
              name="port"
              label="&nbsp;"
              inputTransform={(v) => v.replace(/[\D]/, '')}
              description="Port"
              fieldProps={{
                type: 'number',
                placeholder: 'Port',
                prependLabel: ':',
              }}
            />
          </div>
        </Flex>
      </div>
    </Form>
  );

  return {
    AgentForm,
    watchedValues: {
      port,
      containerName,
      path,
      protocol,
    },
    agentPath,
  };
};
