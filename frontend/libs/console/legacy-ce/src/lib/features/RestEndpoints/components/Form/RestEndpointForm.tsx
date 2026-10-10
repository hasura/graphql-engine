import React from 'react';
import { z } from 'zod';
import { isQueryValid } from '../../utils';
import {
  CheckboxesField,
  CodeEditorField,
  InputField,
  TextAreaField,
  useConsoleForm,
  Button,
  IndicatorCard,
  Text,
  Separator,
} from '@hasura/shared/ui';
import { FaArrowRight, FaMagic } from 'react-icons/fa';
import clsx from 'clsx';
import { openInGraphiQL } from '../../utils';
import { Analytics } from '@hasura/shared/analytics';
import { parse } from 'graphql';
import { useDebouncedEffect } from '@hasura/shared/hooks';
import {
  REST_API_LIST_PATH,
  type RestEndpointFormState,
  type RestEndpointFormSubmitHandler,
} from '../../types';
import { useNavigate } from 'react-router';
import { Flex, Heading } from '@radix-ui/themes';
import { AllowedRestMethods } from '@hasura/shared/types';
import { useAppContext } from '@hasura/shared/context';
import { forgeFormEndpointObject, RestEndpointFormData } from './utils';

const editorOptions = {
  minLines: 10,
  maxLines: 10,
  showLineNumbers: true,
  useSoftTabs: true,
  showPrintMargin: false,
  showGutter: true,
  wrap: true,
};

const formTitle = { create: 'Create', edit: 'Edit' };
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
  onSubmit: RestEndpointFormSubmitHandler;
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
    .refine(
      (val) => isQueryValid(val),
      'Please add a valid named GraphQL query',
    ),
});

export const RestEndpointForm: React.FC<RestEndpointFormProps> = ({
  mode = 'create',
  formState = undefined,
  loading = false,
  onSubmit,
}) => {
  const navigate = useNavigate();
  const { envVars } = useAppContext();

  const resetPageState = () => {
    navigate(REST_API_LIST_PATH);
  };

  const handleSubmit = async (data: Record<string, unknown>) => {
    const state: RestEndpointFormData = {
      name: (data.name as string).trim(),
      // null is respected considering the old Hasura versions <2.10
      comment:
        (data.comment as string) === null
          ? ''
          : (data.comment as string).trim(),
      url: (data.url as string).trim(),
      methods: data.methods as AllowedRestMethods[],
      request: (data.request as string).trim(),
    };
    const [restEndpointObj, request] = forgeFormEndpointObject(state);

    onSubmit(restEndpointObj, request, resetPageState);
  };

  const {
    methods: {
      setFocus,
      watch,
      formState: { errors },
      setValue,
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

  const [userChangedName, setUserChangedName] = React.useState(false);
  const [userChangedUrl, setUserChangedUrl] = React.useState(false);

  useDebouncedEffect(
    () => {
      if (request) {
        try {
          const parsedQuery = parse(request);
          if (parsedQuery?.definitions?.length > 0) {
            const operation = parsedQuery.definitions[0];
            if ('name' in operation) {
              if (!userChangedName && mode === 'create') {
                setValue('name', operation.name?.value ?? '');
              }
              if (!userChangedUrl && mode === 'create') {
                setValue(
                  'url',
                  operation.name?.value
                    ?.toLowerCase()
                    .replace(/[^a-z0-9]+/g, '-')
                    .replace(/(^-|-$)+/g, '') ?? '',
                );
              }
            }
          }
        } catch (e) {
          // do nothing
        }
      }
    },
    500,
    [request],
  );

  const prependLabel = `${envVars.dataApiUrl}/api/rest/`.replace(/\/\//, '/');

  return (
    <Form onSubmit={handleSubmit} className="p-4">
      <div>
        <Heading size="5">{formTitle[mode]} Endpoint</Heading>
        <Text as="p" color="gray">
          {formTitle[mode]} REST Endpoints on top of existing GraphQL queries
          and mutations
        </Text>
      </div>
      <Separator size="4" className="my-2" />
      <div className="space-y-2 w-full md:w-8/12">
        <Flex justify="between">
          <CodeEditorField
            name="request"
            label="GraphQL Request"
            placeholder="Paste GraphQL query here"
            tooltip="The request your endpoint will run. All variables will be mapped to REST endpoint variables."
            description="Support GraphQL queries and mutations."
            editorOptions={editorOptions}
            editorProps={{
              mode: 'graphqlschema',
            }}
          />
          <div className="mt-2">
            <Analytics
              name="api-tab-rest-endpoint-form-graphiql-link"
              passHtmlAttributesToChildren
            >
              <Button
                mode="default"
                rightIcon={FaArrowRight}
                size="sm"
                onClick={(e) => {
                  openInGraphiQL(navigate, request);
                }}
              >
                {request ? 'Test it in ' : 'Import from '} GraphiQL{' '}
              </Button>
            </Analytics>
          </div>
        </Flex>
        <InputField
          name="name"
          label="Name"
          inputTransform={(str) => {
            setUserChangedName(true);
            return str;
          }}
          fieldProps={{
            placeholder: 'Name',
          }}
        />
        <TextAreaField
          name="comment"
          label="Description (Optional)"
          placeholder="Description"
        />
        <div>
          <InputField
            name="url"
            label="URL Path"
            description={`This is the location of your endpoint (must be unique).`}
            inputTransform={(str) => {
              setUserChangedUrl(true);
              return str;
            }}
            fieldProps={{
              placeholder: 'Location',
              prependLabel: prependLabel,
            }}
          />

          <IndicatorCard
            className={clsx(errors?.url ? 'mt-1' : '-mt-4', 'py-3')}
            showIcon
            status="info"
            customIcon={FaMagic}
            headline="Variables can be populated from path components"
          >
            Variables parameterized in the graphql query can be populated from
            the URL path by including the variable name in the template above
            starting with a colon (e.g. {prependLabel}example/:id)
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
        <Flex gap="4">
          <Button
            type="button"
            mode="default"
            onClick={resetPageState}
            disabled={loading}
          >
            Cancel
          </Button>
          <Analytics
            name={`api-tab-rest-endpoint-form-${mode}-button`}
            passHtmlAttributesToChildren
          >
            <Button
              type="submit"
              mode="primary"
              loading={loading}
              loadingText={
                { create: 'Creating...', edit: 'Modifying ..' }[mode]
              }
            >
              {{ create: 'Create', edit: 'Modify' }[mode]}
            </Button>
          </Analytics>
        </Flex>
      </div>
    </Form>
  );
};
