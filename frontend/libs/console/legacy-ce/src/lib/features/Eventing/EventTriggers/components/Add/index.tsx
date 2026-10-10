import React, { useEffect, useReducer } from 'react';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { isProConsole, dataRoutes } from '@hasura/shared/utils';
import {
  requestTransformReducer,
  setEnvVars,
  setSessionVars,
  setRequestMethod,
  setRequestUrl,
  setRequestUrlError,
  setRequestUrlPreview,
  setRequestQueryParams,
  setRequestAddHeaders,
  setRequestBody,
  setRequestBodyError,
  setRequestSampleInput,
  setRequestTransformedBody,
  setRequestContentType,
  setRequestUrlTransform,
  setRequestPayloadTransform,
} from '../../../../ConfigureTransformation/requestTransformState';
import {
  QueryParams,
  RequestTransformStateBody,
} from '../../../../ConfigureTransformation/stateDefaults';
import ConfigureTransformation from '../../../../ConfigureTransformation/ConfigureTransformation';
import { Button } from '@hasura/shared/ui';
import { useEELiteAccess } from '../../../../EETrial';
import {
  getEventRequestSampleInput,
  getEventRequestTransformDefaultState,
  validateEventTriggerState,
} from '../../utils';
import CreateETForm from './CreateETForm';
import { RetryConf, EventTriggerAutoCleanup } from '../../types';
import { useDebouncedEffect, useDocumentTitle } from '@hasura/shared/hooks';
import { hasuraToast } from '@hasura/shared/ui';
import { useNavigate } from 'react-router';
import { useAppContext } from '@hasura/shared/context';
import { useMetadata } from '@hasura/metadata/api';
import useCreateEventTrigger from '../../hooks/useCreateEventTrigger';
import type {
  RequestTransformContentType,
  RequestTransformMethod,
  ClientHeader,
  Table,
} from '@hasura/shared/types';
import useEventTriggerForm from '../../hooks/useEventTriggerForm';
import { useTestWebhookTransform } from '../../../../ConfigureTransformation';
import { EVENTS_SERVICE_HEADING, NameValue } from '@hasura/shared/types';
import { parseValidateApiData } from '../../../../ConfigureTransformation/utils';
import { Heading } from '@radix-ui/themes';
import {
  getDatabaseMethods,
  useTableColumnInfos,
} from '@hasura/metadata/data-source';

const AddEventTrigger: React.FC = () => {
  useDocumentTitle(`Add Event Trigger | ${EVENTS_SERVICE_HEADING} - Hasura`);
  const navigate = useNavigate();
  const { data: dataSourcesList, refetch: refetchMetadata } = useMetadata((m) =>
    m.metadata.sources.filter((s) =>
      getDatabaseMethods(s.kind).check.isFeatureSupported(
        'events.triggers.add',
      ),
    ),
  );
  const { state, setState } = useEventTriggerForm();
  const { name, table, webhook, retryConf, source, operations } = state;
  const { readOnlyMode, envVars } = useAppContext();
  const { access: eeLiteAccess } = useEELiteAccess();
  const createEventTrigger = useCreateEventTrigger();
  const testWebhook = useTestWebhookTransform();

  const currentSource = source
    ? dataSourcesList?.find((s) => s.name === source)
    : undefined;

  const {
    data: columns,
    isFetching: columnFetching,
    error: columnFetchingError,
  } = useTableColumnInfos(
    {
      source: currentSource,
      table,
    },
    {
      enabled: state.operations.includes('UPDATE'),
    },
  );

  const autoCleanupSupport =
    isProConsole(envVars) || eeLiteAccess === 'active'
      ? 'active'
      : eeLiteAccess;

  const [transformState, transformDispatch] = useReducer(
    requestTransformReducer,
    getEventRequestTransformDefaultState(),
  );

  const resetSampleInput = () => {
    const value = getEventRequestSampleInput(
      name,
      table,
      retryConf.num_retries,
      columns,
      operations,
    );
    transformDispatch(setRequestSampleInput(value));
  };

  const envVarsOnChange = (envVars: NameValue[]) => {
    transformDispatch(setEnvVars(envVars));
  };

  const sessionVarsOnChange = (sessionVars: NameValue[]) => {
    transformDispatch(setSessionVars(sessionVars));
  };

  const requestMethodOnChange = (requestMethod: RequestTransformMethod) => {
    transformDispatch(setRequestMethod(requestMethod));
  };

  const requestUrlOnChange = (requestUrl: string) => {
    transformDispatch(setRequestUrl(requestUrl));
  };

  const requestUrlErrorOnChange = (requestUrlError: string) => {
    transformDispatch(setRequestUrlError(requestUrlError));
  };

  const requestUrlPreviewOnChange = (requestUrlPreview: string) => {
    transformDispatch(setRequestUrlPreview(requestUrlPreview));
  };

  const requestQueryParamsOnChange = (requestQueryParams: QueryParams) => {
    transformDispatch(setRequestQueryParams(requestQueryParams));
  };

  const requestAddHeadersOnChange = (requestAddHeaders: NameValue[]) => {
    transformDispatch(setRequestAddHeaders(requestAddHeaders));
  };

  const requestBodyOnChange = (requestBody: RequestTransformStateBody) => {
    transformDispatch(setRequestBody(requestBody));
  };

  const requestBodyErrorOnChange = (requestBodyError: string) => {
    transformDispatch(setRequestBodyError(requestBodyError));
  };

  const requestSampleInputOnChange = (requestSampleInput: string) => {
    transformDispatch(setRequestSampleInput(requestSampleInput));
  };

  const requestTransformedBodyOnChange = (requestTransformedBody: string) => {
    transformDispatch(setRequestTransformedBody(requestTransformedBody));
  };

  const requestContentTypeOnChange = (
    requestContentType: RequestTransformContentType,
  ) => {
    transformDispatch(setRequestContentType(requestContentType));
  };

  const requestUrlTransformOnChange = (data: boolean) => {
    transformDispatch(setRequestUrlTransform(data));
  };

  const requestPayloadTransformOnChange = (data: boolean) => {
    transformDispatch(setRequestPayloadTransform(data));
  };

  useEffect(() => {
    requestUrlErrorOnChange('');
    requestUrlPreviewOnChange('');
    const onResponse = (data: Record<string, any>) => {
      parseValidateApiData(
        data,
        requestUrlErrorOnChange,
        requestUrlPreviewOnChange,
      );
    };
    if (!webhook.value) {
      requestUrlErrorOnChange(
        'Please configure your webhook handler to generate request url transform',
      );
    } else {
      testWebhook({
        version: transformState.version,
        inputPayloadString: transformState.requestSampleInput,
        webhookUrl: webhook.value,
        envVarsFromContext: transformState.envVars,
        sessionVarsFromContext: transformState.sessionVars,
        requestUrl: transformState.requestUrl,
        queryParams: transformState.requestQueryParams,
        isEnvVar: webhook.type === 'env',
      })
        .then(onResponse)
        // parseValidateApiData will parse both success and error
        .catch(onResponse);
    }
  }, [
    transformState.requestSampleInput,
    webhook,
    transformState.requestUrl,
    transformState.requestQueryParams,
    transformState.envVars,
    transformState.sessionVars,
  ]);

  useDebouncedEffect(
    () => {
      requestBodyErrorOnChange('');
      requestTransformedBodyOnChange('');
      const onResponse = (data: Record<string, any>) => {
        parseValidateApiData(
          data,
          requestBodyErrorOnChange,
          undefined,
          requestTransformedBodyOnChange,
        );
      };

      if (!webhook.value) {
        requestBodyErrorOnChange(
          'Please configure your webhook handler to generate request body transform',
        );
      } else if (transformState.requestBody && webhook.value) {
        testWebhook({
          version: transformState.version,
          inputPayloadString: transformState.requestSampleInput,
          webhookUrl: webhook.value,
          envVarsFromContext: transformState.envVars,
          sessionVarsFromContext: transformState.sessionVars,
          transformerBody: transformState.requestBody,
          isEnvVar: webhook.type === 'env',
        })
          .then(onResponse)
          // parseValidateApiData will parse both success and error
          .catch(onResponse);
      }
    },
    1000,
    [
      transformState.requestSampleInput,
      transformState.requestBody,
      webhook,
      transformState.envVars,
      transformState.sessionVars,
    ],
  );

  const createBtnText = 'Create Event Trigger';

  const submit = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();

    if (!currentSource?.kind) {
      return;
    }

    const invalidMessage = validateEventTriggerState(state);
    if (invalidMessage) {
      hasuraToast({
        type: 'error',
        title: 'Creating event trigger failed.',
        message: invalidMessage,
      });
      return;
    }

    const newState = { ...state };

    /* don't cleanup_config if console type is oss */
    if (autoCleanupSupport === 'active') {
      delete newState?.cleanupConfig;
    }

    /* don't pass cleanup config if it's empty or just have only paused*/
    if (
      JSON.stringify(newState?.cleanupConfig) === '{}' ||
      JSON.stringify(newState?.cleanupConfig) === '{"paused":true}'
    ) {
      delete newState?.cleanupConfig;
    }

    createEventTrigger(
      {
        state: newState,
        transformState,
        sourceKind: currentSource.kind,
      },
      () => {
        refetchMetadata().then(() => {
          navigate(dataRoutes.getETModifyRoute({ name: state.name }));
        });
      },
    );
  };

  const handleTriggerNameChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const triggerName = e.target.value;
    setState.name(triggerName);
  };

  const handleDatabaseChange = (database: string) => {
    setState.table(null);
    setState.source(database);
  };

  const handleTableChange = (table: Table) => {
    setState.table(table);
  };

  const handleWebhookTypeChange = (e: React.BaseSyntheticEvent) => {
    const type = e.target.getAttribute('value');
    setState.webhook({
      type,
      value: '',
    });
  };

  const handleWebhookValueChange = (value: string) => {
    setState.webhook({
      type: webhook.type,
      value,
    });
  };

  const handleAutoCleanupChange = (config: EventTriggerAutoCleanup) => {
    setState.cleanupConfig(config);
  };

  const handleRetryConfChange = (r: RetryConf) => {
    setState.retryConf(r);
  };

  const handleHeadersChange = (h: ClientHeader[]) => {
    setState.headers(h);
  };

  return (
    <Analytics name="AddEventTrigger" {...REDACT_EVERYTHING}>
      <div className="w-full overflow-y-auto p-4">
        <div className="max-w-6xl">
          <div className="pt-4 pb-4 clear-both pl-md">
            <div>
              <Heading size="6" className="mb-4">
                Create a new event trigger
              </Heading>
            </div>
            <br />
            <div className="w-full pl-0">
              <form onSubmit={submit}>
                <div className="w-full pl-0">
                  <CreateETForm
                    state={state}
                    columns={columns}
                    currentSource={currentSource}
                    dataSourcesList={dataSourcesList ?? []}
                    readOnlyMode={readOnlyMode}
                    areColumnsFetching={columnFetching}
                    columnsFetchingError={columnFetchingError}
                    handleTriggerNameChange={handleTriggerNameChange}
                    handleWebhookValueChange={handleWebhookValueChange}
                    handleWebhookTypeChange={handleWebhookTypeChange}
                    handleTableChange={handleTableChange}
                    handleDatabaseChange={handleDatabaseChange}
                    handleOperationsChange={setState.operations}
                    handleOperationsColumnsChange={setState.operationColumns}
                    handleRetryConfChange={handleRetryConfChange}
                    handleHeadersChange={handleHeadersChange}
                    toggleAllColumnChecked={setState.toggleAllColumnChecked}
                    handleAutoCleanupChange={handleAutoCleanupChange}
                    autoCleanupSupport={autoCleanupSupport}
                  />
                  <div className="my-4">
                    <ConfigureTransformation
                      transformationType="event"
                      requestTransformState={transformState}
                      resetSampleInput={resetSampleInput}
                      envVarsOnChange={envVarsOnChange}
                      sessionVarsOnChange={sessionVarsOnChange}
                      requestMethodOnChange={requestMethodOnChange}
                      requestUrlOnChange={requestUrlOnChange}
                      requestQueryParamsOnChange={requestQueryParamsOnChange}
                      requestAddHeadersOnChange={requestAddHeadersOnChange}
                      requestBodyOnChange={requestBodyOnChange}
                      requestSampleInputOnChange={requestSampleInputOnChange}
                      requestContentTypeOnChange={requestContentTypeOnChange}
                      requestUrlTransformOnChange={requestUrlTransformOnChange}
                      requestPayloadTransformOnChange={
                        requestPayloadTransformOnChange
                      }
                    />
                  </div>
                  {!readOnlyMode && (
                    <Analytics
                      name="events-tab-button-create-event-trigger"
                      passHtmlAttributesToChildren
                    >
                      <Button
                        type="submit"
                        mode="primary"
                        data-test="trigger-create"
                      >
                        {createBtnText}
                      </Button>
                    </Analytics>
                  )}
                </div>
              </form>
            </div>
          </div>
        </div>
      </div>
    </Analytics>
  );
};

export default AddEventTrigger;
