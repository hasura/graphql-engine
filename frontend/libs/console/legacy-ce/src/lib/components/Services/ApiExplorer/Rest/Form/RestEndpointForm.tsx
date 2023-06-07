import React from 'react';
import { z } from 'zod';

import { AllowedRESTMethods } from '../../../../../metadata/types';
import { isQueryValid } from '../utils';
import {
  CheckboxesField,
  CodeEditorField,
  InputField,
  Textarea,
  useConsoleForm,
} from '../../../../../new-components/Form';
import { Button } from '../../../../../new-components/Button';
import globals from '../../../../../Globals';

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
    .enum(['GET', 'POST', 'PUT', 'PATCH', 'DELETE'])
    .array()
    .nonempty({ message: 'Choose at least one method' }),
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
  const {
    methods: { setFocus },
    Form,
  } = useConsoleForm({
    schema: validationSchema,
    options: {
      defaultValues: formState,
    },
  });

  React.useEffect(() => {
    setFocus('name');
  }, []);

  const prependLabel = `${globals.dataApiUrl}/api/rest/`.replace(/\/\//, '/');

  return (
    <Form onSubmit={onSubmit} className="p-9">
      <div className="space-y-2 w-full max-w-xl">
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
          description={`This is the location of your endpoint (must be unique). Any parameterized variables (eg. ${prependLabel}example/:id will be made available to your request.`}
          prependLabel={prependLabel}
        />
        <CheckboxesField
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
            loadingText={{ create: 'Creating...', edit: 'Modifying ..' }[mode]}
          >
            {{ create: 'Create', edit: 'Modify' }[mode]}
          </Button>
        </div>
      </div>
    </Form>
  );
};
