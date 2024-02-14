import { Button } from '../../../new-components/Button';
import { InputField, SimpleForm } from '../../../new-components/Form';
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
  const { addAgent, isLoading } = useAddAgent();

  const handleSubmit = (values: FormValues) => {
    addAgent({
      name: values.name,
      url:
        values.url.type === 'envVar'
          ? { from_env: values.url.value }
          : values.url.value,
    }).then(response => {
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
      <div className="bg-white p-6 border border-gray-300 rounded space-y-4 mb-6 max-w-xl">
        <p className="text-lg text-gray-600 font-bold">
          Connect a Data Connector Agent
        </p>
        <hr />

        <InputField
          label="Name"
          name="name"
          type="text"
          tooltip="This value will be used as the source kind in metadata"
          placeholder="Enter the name of the agent"
        />

        <UrlInput />

        <div className="flex gap-4 justify-end">
          <Button type="submit" mode="primary" isLoading={isLoading}>
            Connect
          </Button>
          <Button
            onClick={() => {
              props.onClose();
            }}
          >
            Close
          </Button>
        </div>
      </div>
    </SimpleForm>
  );
};

AddAgentForm.defaultProps = {
  onSuccess: () => {},
};
