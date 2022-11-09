import { Button } from '@/new-components/Button';
import { Form, InputField } from '@/new-components/Form';
import React from 'react';
import { z } from 'zod';
import { useAddAgent } from '../hooks/useAddAgent';

interface CreateAgentFormProps {
  onClose: () => void;
  onSuccess?: () => void;
}

const schema = z.object({
  name: z.string().min(1, 'Name is required!'),
  url: z.string().min(1, 'URL is required!'),
});

type FormValues = z.infer<typeof schema>;

export const AddAgentForm = (props: CreateAgentFormProps) => {
  const { addAgent, isLoading } = useAddAgent();

  const handleSubmit = (values: FormValues) => {
    addAgent({
      ...values,
      onSuccess: props.onSuccess,
    });
  };

  return (
    <Form
      schema={schema}
      // something is wrong with type inference with react-hook-form form wrapper. temp until the issue is resolved
      onSubmit={handleSubmit as any}
      options={{ defaultValues: { url: '', name: '' } }}
      className="p-0 py-4"
    >
      {() => {
        return (
          <>
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

              <InputField
                label="URL"
                name="url"
                type="text"
                tooltip="The URL of the data connector agent"
                placeholder="Enter the URI of the agent"
              />
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
          </>
        );
      }}
    </Form>
  );
};

AddAgentForm.defaultProps = {
  onSuccess: () => {},
};
