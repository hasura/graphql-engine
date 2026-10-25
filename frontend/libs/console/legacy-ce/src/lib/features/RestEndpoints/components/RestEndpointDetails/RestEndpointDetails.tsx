import React, { useEffect } from 'react';
import { Flex } from '@radix-ui/themes';
import { z } from 'zod';
import {
  CheckboxesField,
  CodeEditorField,
  InputField,
  TextAreaField,
  useConsoleForm,
  Button,
  hasuraToast,
  showErrorNotification,
} from '@hasura/shared/ui';
import { FaArrowRight, FaPlay } from 'react-icons/fa';
import { useRestEndpointRequest } from '../../hooks/useRestEndpointRequest';
import { RequestHeaders } from './RequestHeaders';
import { Variables } from './Variables';
import { openInGraphiQL } from '../../utils';
import { Analytics } from '@hasura/shared/analytics';
import { useAuthContext } from '@hasura/shared/context';
import { DataHeader } from '@hasura/shared/types';
import { useNavigate } from 'react-router';
import { parseQueryVariables } from '../../../ApiExplorer/components/Rest/utils';
import { getPersistedGraphiQLHeaders } from '../../../ApiExplorer/components/ApiRequest/utils';
import { useRestEndpoint } from '@hasura/metadata/api';
import { AllowedRestMethods } from '@hasura/shared/types';

export type Variable = Exclude<
  ReturnType<typeof parseQueryVariables>,
  undefined
>[0] & {
  value: string;
};

export type RestEndpointDetailsProps = {
  name: string;
};

const commonEditorOptions = {
  showLineNumbers: true,
  useSoftTabs: true,
  showPrintMargin: false,
  showGutter: true,
  wrap: true,
};

const requestEditorOptions = {
  ...commonEditorOptions,
  minLines: 10,
  maxLines: 10,
};

const responseEditorOptions = {
  ...commonEditorOptions,
  minLines: 50,
  maxLines: 50,
};

const validationSchema = z.object({
  name: z.string().min(1, { message: 'Please add a name' }),
  comment: z.union([z.string(), z.null()]),
  url: z.string().min(1, { message: 'Please add a location' }),
  methods: z
    .enum(['GET', 'POST', 'PUT', 'PATCH', 'DELETE'])
    .array()
    .nonempty({ message: 'Choose at least one method' }),
  request: z.string().min(1, { message: 'Please add a GraphQL query' }),
  response: z.string().nullish(),
});

export const RestEndpointDetails = (props: RestEndpointDetailsProps) => {
  const navigate = useNavigate();
  const endpoint = useRestEndpoint(props.name);
  const { getHeaders } = useAuthContext();

  const [headers, setHeaders] = React.useState<DataHeader[]>([]);
  const [variables, setVariables] = React.useState<Variable[]>([]);
  const { data, mutate, isPending: isLoading } = useRestEndpointRequest();

  const {
    Form,
    methods: { setValue },
  } = useConsoleForm({
    schema: validationSchema,
  });

  // Seed the request headers once on mount. `getHeaders` is recreated on every
  // render by the auth context, so we deliberately read the latest one through a
  // ref to keep this a mount-only fetch (re-running it would discard the user's
  // header edits).
  const getHeadersRef = React.useRef(getHeaders);
  useEffect(() => {
    getHeadersRef.current().then((authHeaders) => {
      const initialHeaders = getPersistedGraphiQLHeaders(authHeaders);
      setHeaders(
        initialHeaders.map((header) => ({
          ...header,
          selected: true,
        })),
      );
    });
  }, []);

  // `useRestEndpoint` returns a new wrapper object each render, but its
  // `.endpoint`/`.query` sub-objects are stable (react-query). Depend on those
  // stable values instead of the wrapper to avoid re-running on every render.
  const endpointData = endpoint?.endpoint;
  const queryString = endpoint?.query?.query;

  // Reset the editable variables while rendering whenever the endpoint's query
  // changes (adjusting state during render instead of in an effect to avoid a
  // cascading render; the variables remain user-editable afterwards).
  const [prevQueryString, setPrevQueryString] = React.useState<
    string | undefined
  >(undefined);
  if (queryString !== prevQueryString) {
    setPrevQueryString(queryString);
    if (queryString) {
      const parsedVariables = parseQueryVariables(queryString);
      setVariables(parsedVariables?.map((v) => ({ ...v, value: '' })) ?? []);
    }
  }

  useEffect(() => {
    if (endpointData) {
      setValue('name', endpointData?.name);
      setValue('comment', endpointData?.comment ?? null);
      setValue('url', endpointData?.url);
      setValue('methods', (endpointData?.methods ?? []) as any);
      setValue('request', queryString ?? '');
    }
  }, [endpointData, queryString, setValue]);

  useEffect(() => {
    setValue('response', JSON.stringify(data, null, 2));
  }, [data, setValue]);

  if (!endpoint) {
    return null;
  }

  return (
    <Form
      onSubmit={() => {
        /* noop */
      }}
    >
      <div className="grid grid-cols-2 gap-4">
        <Flex direction="column" gap="2">
          <div className="relative">
            <CodeEditorField
              disabled
              editorOptions={requestEditorOptions}
              description="Support GraphQL queries and mutations."
              name="request"
              label="GraphQL Request"
            />

            <div className="text-sm absolute top-3 right-0 mt-2">
              <Analytics
                name="api-tab-rest-endpoint-details-graphiql-linkt"
                passHtmlAttributesToChildren
              >
                <Button
                  rightIcon={FaArrowRight}
                  size="sm"
                  onClick={(e) => {
                    openInGraphiQL(navigate, endpoint.query.query);
                  }}
                >
                  Test it in GraphiQL
                </Button>
              </Analytics>
            </div>
          </div>
          <TextAreaField
            disabled
            name="comment"
            label="Description"
            placeholder="Description"
          />
          <InputField
            name="url"
            label="Location"
            description={`This is the location of your endpoint (must be unique). Any parameterized variables`}
            fieldProps={{
              disabled: true,
              placeholder: 'Location',
            }}
          />
          <CheckboxesField
            disabled
            name="methods"
            label="Methods"
            options={[
              { value: 'GET', label: 'GET' },
              { value: 'POST', label: 'POST' },
              { value: 'PUT', label: 'PUT' },
              { value: 'PATCH', label: 'PATCH' },
              { value: 'DELETE', label: 'DELETE' },
            ].filter(({ value }) =>
              endpoint.endpoint?.methods.includes(value as AllowedRestMethods),
            )}
            orientation="horizontal"
          />
          <Variables variables={variables} setVariables={setVariables} />

          <RequestHeaders headers={headers} setHeaders={setHeaders} />

          <div className="mt-2">
            <Analytics
              name="api-tab-rest-endpoint-details-run-request"
              passHtmlAttributesToChildren
            >
              <Button
                disabled={!endpoint?.endpoint}
                loading={isLoading}
                leftIcon={FaPlay}
                onClick={() => {
                  mutate(
                    {
                      endpoint: endpoint?.endpoint,
                      headers,
                      variables,
                    },
                    {
                      onSuccess: (data) => {
                        hasuraToast({
                          title: 'Success',
                          message: 'Request successful',
                          type: 'success',
                        });
                        window.scrollTo({
                          top: 0,
                          behavior: 'smooth',
                        });
                      },
                      onError: (error) => {
                        showErrorNotification({
                          message: 'Request failed',
                          error,
                        });
                      },
                    },
                  );
                }}
                mode="primary"
              >
                Run Request
              </Button>
            </Analytics>
          </div>
        </Flex>
        <div>
          <CodeEditorField
            editorOptions={responseEditorOptions}
            name="response"
            label="GraphQL Response"
            editorProps={{
              mode: 'json',
            }}
          />
        </div>
      </div>
    </Form>
  );
};
