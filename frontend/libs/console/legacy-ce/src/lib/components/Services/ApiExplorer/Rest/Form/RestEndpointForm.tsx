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
import { FaArrowRight, FaMagic } from 'react-icons/fa';
import { IndicatorCard } from '../../../../../new-components/IndicatorCard';
import clsx from 'clsx';
import { Link } from 'react-router';

const editorOptions = {
  minLines: 10,
  maxLines: 10,
  showLineNumbers: true,
  useSoftTabs: true,
  showPrintMargin: false,
  showGutter: true,
  wrap: true,
};

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
    methods: {
      setFocus,
      watch,
      formState: { errors },
    },
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

  const request = watch('request');

  const prependLabel = `${globals.dataApiUrl}/api/rest/`.replace(/\/\//, '/');

  return (
    <Form onSubmit={onSubmit} className="p-9">
      <div className="pb-2">
        <h1 className="text-xl font-semibold mb-0">
          {{ create: 'Create', edit: 'Edit' }[mode]} Endpoint
        </h1>
        <p className="text-gray-500 mt-0">
          {{ create: 'Create', edit: 'Edit' }[mode]} REST Endpoints on top of
          existing GraphQL queries and mutations
        </p>
        <hr className="mb-4 mt-2 -mx-9" />
      </div>
      <div className="space-y-2 w-full max-w-3xl">
        <div className="relative">
          <CodeEditorField
            name="request"
            label="GraphQL Request"
            placeholder="Paste GraphQL query here"
            tooltip="The request your endpoint will run. All variables will be mapped to REST endpoint variables."
            description="Support GraphQL queries and mutations."
            editorOptions={editorOptions}
          />
          <div className="text-sm absolute top-6 right-0 mt-2 mr-2">
            <Link to="/api/api-explorer?mode=rest">
              {request ? 'Test it in ' : 'Import from '} GraphiQL{' '}
              <FaArrowRight />
            </Link>
          </div>
        </div>
        <InputField name="name" label="Name" placeholder="Name" />
        <Textarea
          name="comment"
          label="Description (Optional)"
          placeholder="Description"
        />
        <div>
          <InputField
            name="url"
            label="URL Path"
            placeholder="Location"
            description={`This is the location of your endpoint (must be unique).`}
            prependLabel={prependLabel}
          />
          <IndicatorCard
            className={clsx(errors?.url ? 'mt-1' : '-mt-4', 'py-3')}
            showIcon
            status="info"
            customIcon={FaMagic}
            headline="No Parameterized variable specification needed"
          >
            All parameterized variables in your GraphQL query will be
            auto-specifed in the URL
          </IndicatorCard>
        </div>
        <CheckboxesField
          name="methods"
          label="Methods"
          options={[
            { value: 'GET', label: 'GET' },
            { value: 'POST', label: 'POST' },
            { value: 'PUT', label: 'PUT' },
            { value: 'PATCH', label: 'PATCH' },
            { value: 'DELETE', label: 'DELETE' },
          ]}
          orientation="horizontal"
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
