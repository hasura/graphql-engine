import React from 'react';
import { z } from 'zod';

import { AllowedRESTMethods } from '@/metadata/types';
import { isQueryValid } from '@/components/Services/ApiExplorer/Rest/utils';
import {
  Form,
  InputField,
  Textarea,
  CodeEditorField,
  Checkbox,
} from '@/new-components/Form';
import { Button } from '@/new-components/Button';
import { UseFormReturn } from 'react-hook-form';

type RestEndpointFormProps = {
  /**
   * The form mode
   */
  mode: 'create' | 'edit';
  /**
   * The form initial state
   */
  formState: RestEndpointFormState | undefined;
  /**
   * Flag indicating wheter the form is loading
   */
  loading: boolean;
  /**
   * Handler for the submit event
   */
  onSubmit: (data: Record<string, unknown>) => void;
  /**
   * Handler for the cancel event
   */
  onCancel: () => void;
};

export type RestEndpointFormState = {
  name?: string;
  comment?: string;
  url?: string;
  methods?: AllowedRESTMethods[];
  request?: string;
};

export type RestEndpointFormData = {
  name: string;
  comment?: string;
  url: string;
  methods: AllowedRESTMethods[];
  request: string;
};

const validationSchema = z.object({
  name: z.string().min(1, { message: 'Please add a name' }),
  comment: z.union([z.string(), z.null()]),
  url: z.string().min(1, { message: 'Please add a location' }),
  methods: z
    // When nothing is selected, the value is a false boolean
    .union([z.string().array(), z.boolean()])
    .refine(
      value => Array.isArray(value) && value.length > 0,
      'Choose at least one method'
    ),
  request: z
    .string()
    .min(1, { message: 'Please add a GraphQL query' })
    .refine(val => isQueryValid(val), 'Please add a valid GraphQL query'),
});

export const RestEndpointForm: React.FC<RestEndpointFormProps> = ({
  mode = 'create',
  formState = undefined,
  loading = false,
  onSubmit = () => {},
  onCancel = () => {},
}) => {
  const restEndpointFormRef = React.useRef<UseFormReturn>(null);
  setTimeout(() => {
    restEndpointFormRef.current?.setFocus('name');
  }, 100);
  return (
    <Form
      ref={restEndpointFormRef}
      schema={validationSchema}
      options={{
        defaultValues: formState,
      }}
      onSubmit={onSubmit}
    >
      {() => (
        <div className="space-y-2">
          <h1 className="text-xl font-semibold mb-sm">
            {{ create: 'Create', edit: 'Edit' }[mode]} Endpoint
          </h1>
          <InputField name="name" label="Name *" placeholder="Name" />
          <Textarea
            name="comment"
            label="Description"
            placeholder="Description"
          />
          <InputField
            name="url"
            label="Location *"
            placeholder="Location"
            description="This is the location of your endpoint (must be unique). Any parameterized variables (eg. http://localhost:8080/api/rest/example/:id will be made available to your request."
            prependLabel="http://localhost:8080/api/rest/"
          />
          <Checkbox
            name="methods"
            label="Methods *"
            options={[
              { value: 'GET', label: 'GET' },
              { value: 'POST', label: 'POST' },
              { value: 'PUT', label: 'PUT' },
              { value: 'PATCH', label: 'PATCH' },
              { value: 'DELETE', label: 'DELETE' },
            ]}
            orientation="horizontal"
          />
          <CodeEditorField
            name="request"
            label="GraphQL Request *"
            tooltip="The request your endpoint will run. All variables will be mapped to REST endpoint variables."
          />
          <div className="flex gap-4">
            <Button type="button" onClick={onCancel} disabled={loading}>
              Cancel
            </Button>
            <Button
              type="submit"
              mode="primary"
              isLoading={loading}
              loadingText={
                { create: 'Creating...', edit: 'Modifying ..' }[mode]
              }
            >
              {{ create: 'Create', edit: 'Modify' }[mode]}
            </Button>
          </div>
        </div>
      )}
    </Form>
  );
};
