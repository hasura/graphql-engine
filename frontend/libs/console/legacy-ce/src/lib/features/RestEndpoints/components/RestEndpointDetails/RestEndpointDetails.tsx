import React, { useEffect } from 'react';
import { z } from 'zod';
import AceEditor from 'react-ace';
import {
  CheckboxesField,
  CodeEditorField,
  InputField,
  Textarea,
  useConsoleForm,
} from '../../../../new-components/Form';
import { Button } from '../../../../new-components/Button';
import { FaArrowRight, FaPlay } from 'react-icons/fa';
import { useRestEndpoint } from '../../hooks/useRestEndpoint';
import { parseQueryVariables } from '../../../../components/Services/ApiExplorer/Rest/utils';
import { useRestEndpointRequest } from '../../hooks/useRestEndpointRequest';
import { RequestHeaders } from './RequestHeaders';
import { Variables } from './Variables';
import { AllowedRESTMethods } from '../../../../metadata/types';
import { openInGraphiQL } from './utils';
import { LS_KEYS, getLSItem } from '../../../../utils';
import { hasuraToast } from '../../../../new-components/Toasts';
import { Analytics } from '../../../Analytics';

export type Variable = Exclude<
  ReturnType<typeof parseQueryVariables>,
  undefined
>[0] & {
  value: string;
};

export type Header = {
  name: string;
  value: string;
  selected: boolean;
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
});

export const getInitialHeaders = (): Header[] => {
  const headers = getLSItem(LS_KEYS.apiExplorerConsoleGraphQLHeaders);

  if (headers) {
    return JSON.parse(headers).map(
      (header: { key: string; value: string; isDisabled: boolean }) => {
        const value =
          header.key === 'x-hasura-admin-secret'
            ? window.__env.adminSecret || getLSItem(LS_KEYS.consoleAdminSecret)
            : header.value;
        return {
          name: header.key,
          value,
          selected: !header.isDisabled,
        };
      }
    );
  }
  return [
    {
      name: '',
      value: '',
      selected: true,
    },
  ];
};

export const RestEndpointDetails = (props: RestEndpointDetailsProps) => {
  const endpoint = useRestEndpoint(props.name);

  const initialHeaders = getInitialHeaders();

  const [headers, setHeaders] = React.useState(
    initialHeaders.map(header => ({
      ...header,
      selected: true,
    }))
  );

  const [variables, setVariables] = React.useState<Variable[]>([]);

  const { data, mutate, isLoading } = useRestEndpointRequest();

  const {
    Form,
    methods: { setValue },
  } = useConsoleForm({
    schema: validationSchema,
  });

  useEffect(() => {
    if (endpoint?.query?.query) {
      const parsedVariables = parseQueryVariables(endpoint.query.query);
      setVariables(parsedVariables?.map(v => ({ ...v, value: '' })) ?? []);
    }

    if (endpoint) {
      setValue('name', endpoint.endpoint?.name);
      setValue('comment', endpoint?.endpoint?.comment);
      setValue('url', endpoint?.endpoint?.url);
      setValue('methods', endpoint?.endpoint?.methods);
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
        <div className="flex flex-col gap-2">
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
                  icon={<FaArrowRight />}
                  iconPosition="end"
                  size="sm"
                  onClick={e => {
                    openInGraphiQL(endpoint.query.query);
                  }}
                >
                  Test it in GraphiQL
                </Button>
              </Analytics>
            </div>
          </div>
          <Textarea
            disabled
            name="comment"
            label="Description"
            placeholder="Description"
          />
          <InputField
            disabled
            name="url"
            label="Location"
            placeholder="Location"
            description={`This is the location of your endpoint (must be unique). Any parameterized variables`}
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
              endpoint.endpoint?.methods.includes(value as AllowedRESTMethods)
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
                isLoading={isLoading}
                icon={<FaPlay />}
                onClick={() => {
                  mutate(
                    {
                      endpoint: endpoint?.endpoint,
                      headers,
                      variables,
                    },
                    {
                      onSuccess: data => {
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
                      onError: error => {
                        hasuraToast({
                          title: 'Error',
                          message: 'Request failed',
                          type: 'error',
                          children: (
                            <div className="overflow-hidden">
                              <AceEditor
                                theme="github"
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
                    }
                  );
                }}
                mode="primary"
              >
                Run Request
              </Button>
            </Analytics>
          </div>
        </div>
        <div>
          <CodeEditorField
            editorOptions={responseEditorOptions}
            name="response"
            label="GraphQL Response"
          />
        </div>
      </div>
    </Form>
  );
};
