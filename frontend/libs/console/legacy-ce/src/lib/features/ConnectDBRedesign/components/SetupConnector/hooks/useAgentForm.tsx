import React from 'react';
import { MdOutlineTipsAndUpdates } from 'react-icons/md';
import { z } from 'zod';
import { DropDown } from '../../../../../new-components/AdvancedDropDown';
import { InputField, useConsoleForm } from '../../../../../new-components/Form';
import { Nullable } from '../../../../../types';

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

  const [container, setContainer] = React.useState<Nullable<HTMLDivElement>>();

  const AgentForm = () => (
    <Form onSubmit={data => {}}>
      <div ref={r => setContainer(r)}>
        <div className="mb-2 text-muted">
          <MdOutlineTipsAndUpdates className="mr-1" /> Changing these values
          will dynamically alter the install command.
        </div>
        <div className="flex flex-row">
          <InputField
            type="text"
            name="path"
            prependLabel={
              <DropDown.Root
                trigger={<span>{protocol}://</span>}
                container={container}
                side="right"
                align="end"
              >
                <DropDown.RadioGroup
                  value={protocol}
                  label="Protocol"
                  onValueChange={value => {
                    setValue('protocol', value as 'http' | 'https');
                  }}
                >
                  <DropDown.RadioItem value="http">http</DropDown.RadioItem>
                  <DropDown.RadioItem value="https">https</DropDown.RadioItem>
                </DropDown.RadioGroup>
              </DropDown.Root>
            }
            label="Network Path"
            tooltip="This is the network path that Hasura will use to communicate with the Data Connector Service."
            inputTransform={v => v.replace(' ', '')}
            inputClassName="rounded-none border-r-0 before:content-['Hello'] before:text-sky-300 before:text-4xl after:content-['Goodbye'] after:text-amber-300 after:text-4xl"
            placeholder="127.0.0.1"
            description="Protocol &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Host Name / IP Address"
          />
          <style>{`.docker-config-port span.prepend-label { border-radius: 0; }`}</style>
          <InputField
            type="number"
            name="port"
            className="w-48 docker-config-port"
            label="&nbsp;"
            inputClassName="rounded-l-none"
            inputTransform={v => v.replace(/[\D]/, '')}
            placeholder="Port"
            description="Port"
            prependLabel=":"
          />
        </div>
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
