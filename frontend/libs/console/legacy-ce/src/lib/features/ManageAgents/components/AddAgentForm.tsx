import {
  Button,
  Card,
  InputField,
  Separator,
  SimpleForm,
} from '@hasura/shared/ui';
import { Flex, Heading } from '@radix-ui/themes';

import { z } from 'zod';
import { useAddAgent } from '../hooks/useAddAgent';
import { UrlInput } from './UrlInput';

interface CreateAgentFormProps {
  onClose: () => void;
  onSuccess?: () => void;
}

export const schema = z.object({
  name: z.string().min(1, 'Name is required!'),
  url: z.discriminatedUnion('type', [
    z.object({
      type: z.literal('url'),
      value: z.string().min(1, 'URL is required!'),
    }),
    z.object({
      type: z.literal('envVar'),
      value: z.string().min(1, 'ENV variable is required'),
    }),
  ]),
});

export type FormValues = z.infer<typeof schema>;

export const AddAgentForm = (props: CreateAgentFormProps) => {
  const { addAgent, isPending } = useAddAgent();

  const handleSubmit = (values: FormValues) => {
    addAgent({
      name: values.name,
      url:
        values.url.type === 'envVar'
          ? { from_env: values.url.value }
          : values.url.value,
    }).then((response) => {
      response.makeToast();
      if (response.status === 'added') {
        props?.onSuccess?.();
      }
    });
  };

  return (
    <SimpleForm
      schema={schema}
      // something is wrong with type inference with react-hook-form form wrapper. temp until the issue is resolved
      onSubmit={handleSubmit}
      options={{
        defaultValues: { url: { type: 'envVar', value: '' }, name: '' },
      }}
      className="py-4"
    >
      <Card size="1" className="md:w-8/12">
        <Heading size="3">Connect a Data Connector Agent</Heading>
        <Separator size="4" className="my-4" />
        <InputField
          label="Name"
          name="name"
          tooltip="This value will be used as the source kind in metadata"
          fieldProps={{
            type: 'text',
            placeholder: 'Enter the name of the agent',
          }}
        />

        <UrlInput />

        <Flex gap="4" align="center" justify="end">
          <Button
            color="gray"
            variant="ghost"
            onClick={() => {
              props.onClose();
            }}
          >
            Close
          </Button>
          <Button type="submit" mode="primary" loading={isPending}>
            Connect
          </Button>
        </Flex>
      </Card>
    </SimpleForm>
  );
};
