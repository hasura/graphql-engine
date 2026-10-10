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
  AceEditor,
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

  useEffect(() => {
    getHeaders().then((authHeaders) => {
      const initialHeaders = getPersistedGraphiQLHeaders(authHeaders);
      setHeaders(
        initialHeaders.map((header) => ({
          ...header,
          selected: true,
        })),
      );
    });
  }, []);

  useEffect(() => {
    if (endpoint?.query?.query) {
      const parsedVariables = parseQueryVariables(endpoint.query.query);
      setVariables(parsedVariables?.map((v) => ({ ...v, value: '' })) ?? []);
    }

    if (endpoint) {
      setValue('name', endpoint.endpoint?.name);
      setValue('comment', endpoint?.endpoint?.comment ?? null);
      setValue('url', endpoint?.endpoint?.url);
      setValue('methods', (endpoint?.endpoint?.methods ?? []) as any);
      setValue('request', endpoint?.query?.query);
    }
  }, [endpoint?.query?.query, endpoint?.endpoint]);

  useEffect(() => {
    setValue('response', JSON.stringify(data, null, 2));
  }, [data]);

  if (!endpoint) {
    return null;
  }

  return (
    <Form onSubmit={() => {}}>
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
                        hasuraToast({
                          title: 'Error',
                          message: 'Request failed',
                          type: 'error',
                          children: (
                            <div className="overflow-hidden">
                              <AceEditor
                                setOptions={{
                                  minLines: 1,
                                  maxLines: Infinity,
                                  showGutter: false,
                                  useWorker: false,
                                }}
                                value={JSON.stringify(error, null, 2)}
                              />
                            </div>
                          ),
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
