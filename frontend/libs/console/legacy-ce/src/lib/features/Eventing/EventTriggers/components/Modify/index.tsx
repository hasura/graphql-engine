import React, { useEffect, useReducer } from 'react';
import { useNavigate } from 'react-router';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
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
  setRequestSampleInput,
  setRequestTransformedBody,
  setRequestContentType,
  setRequestUrlTransform,
  setRequestTransformState,
  setRequestPayloadTransform,
} from '../../../../ConfigureTransformation/requestTransformState';
import {
  QueryParams,
  RequestTransformStateBody,
} from '../../../../ConfigureTransformation/stateDefaults';
import ConfigureTransformation from '../../../../ConfigureTransformation/ConfigureTransformation';
import { useEELiteAccess } from '../../../../EETrial';
import {
  parseValidateApiData,
  getTransformState,
} from '../../../../ConfigureTransformation/utils';
import { Button, Separator } from '@hasura/shared/ui';
import { isProConsole, dataRoutes } from '@hasura/shared/utils';
import {
  getEventRequestSampleInput,
  getEventRequestTransformDefaultState,
  validateEventTriggerState,
} from '../../utils';
import TableHeader from '../TableCommon/TableHeader';
import { parseServerETDefinition } from '../../utils';
import Info from './Info';
import WebhookEditor from './WebhookEditor';
import { OperationEditor } from '../form/OperationEditor';
import RetryConfEditor from './RetryConfEditor';
import HeadersEditor from './HeadersEditor';
import { AutoCleanupForm } from '../form/AutoCleanupForm';
import { useDebouncedEffect } from '@hasura/shared/hooks';
import { hasuraToast } from '@hasura/shared/ui';
import { useAppContext } from '@hasura/shared/context';
import { useEventTriggerDetailContext } from '../../context';
import useDeleteEventTrigger from '../../hooks/useDeleteEventTrigger';
import useModifyEventTrigger, {
  EventTriggerProperty,
} from '../../hooks/useModifyEventTrigger';
import useEventTriggerForm from '../../hooks/useEventTriggerForm';
import {
  RequestTransformContentType,
  RequestTransformMethod,
} from '@hasura/shared/types';
import { NameValue } from '@hasura/shared/types';
import { useTestWebhookTransform } from '../../../../ConfigureTransformation';
import { useTableColumns } from '@hasura/metadata/data-source';
import { Flex, Heading } from '@radix-ui/themes';

const ModifyEventTrigger: React.FC = () => {
  const navigate = useNavigate();
  const testWebhookTransform = useTestWebhookTransform();
  const { readOnlyMode, envVars } = useAppContext();
  const { eventTrigger, currentTable, currentSource } =
    useEventTriggerDetailContext();
  const {
    data: columns = [],
    isFetching: areColumnsFetching,
    error: columnsFetchingError,
  } = useTableColumns(
    {
      source: currentSource,
      table: currentTable,
    },
    {
      select: (data) => data.columns,
    },
  );

  const { state, setState } = useEventTriggerForm(
    parseServerETDefinition({
      eventTrigger,
      columns,
      table: currentTable,
      source: currentSource.name,
    }),
  );
  const [transformState, transformDispatch] = useReducer(
    requestTransformReducer,
    getEventRequestTransformDefaultState(),
  );
  const { access: eeLiteAccess } = useEELiteAccess();
  const deleteEventTrigger = useDeleteEventTrigger();
  const modifyEventTrigger = useModifyEventTrigger();
  const autoCleanupSupport =
    isProConsole(envVars) || eeLiteAccess === 'active'
      ? 'active'
      : eeLiteAccess;

  useEffect(() => {
    if (eventTrigger.request_transform) {
      const sampleInput = getEventRequestSampleInput(
        eventTrigger.name,
        currentTable,
        eventTrigger.retry_conf?.num_retries,
        columns,
        state.operations,
      );
      transformDispatch(
        setRequestTransformState(
          getTransformState(eventTrigger.request_transform, sampleInput),
        ),
      );
    } else {
      transformDispatch(
        setRequestTransformState(getEventRequestTransformDefaultState()),
      );
    }
  }, [state.name]);

  const resetSampleInput = () => {
    const value = getEventRequestSampleInput(
      eventTrigger.name,
      currentTable,
      eventTrigger.retry_conf.num_retries,
      columns,
      state.operations,
    );
    transformDispatch(setRequestSampleInput(value));
  };

  useEffect(() => {
    resetSampleInput();
  }, [state.webhook?.value]);

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

  useDebouncedEffect(
    () => {
      if (!state.operations.length) {
        return;
      }

      requestUrlErrorOnChange('');
      requestUrlPreviewOnChange('');

      const onResponse = (data: Record<string, any>) => {
        parseValidateApiData(
          data,
          requestUrlErrorOnChange,
          requestUrlPreviewOnChange,
          requestTransformedBodyOnChange,
        );
      };

      if (!state.webhook?.value) {
        requestUrlErrorOnChange(
          'Please configure your webhook handler to generate request transform',
        );
      } else {
        testWebhookTransform({
          version: transformState.version,
          inputPayloadString: transformState.requestSampleInput,
          webhookUrl: state.webhook?.value,
          envVarsFromContext: transformState.envVars,
          sessionVarsFromContext: transformState.sessionVars,
          requestUrl: transformState.requestUrl,
          queryParams: transformState.requestQueryParams,
          isEnvVar: state.webhook.type === 'env',
        })
          .then(onResponse)
          // parseValidateApiData will parse both success and error
          .catch(onResponse);
      }
    },
    1000,
    [
      transformState.requestSampleInput,
      state.webhook?.value,
      transformState.requestUrl,
      transformState.requestQueryParams,
      transformState.envVars,
      transformState.sessionVars,
      transformState.requestTransformedBody,
      transformState.requestBody,
    ],
  );

  const saveWrapper =
    (property?: EventTriggerProperty) =>
    (successCb?: () => void, errorCb?: () => void) => {
      const invalidMessage = validateEventTriggerState(state);
      if (invalidMessage) {
        hasuraToast({
          type: 'error',
          title: 'Updating event trigger failed.',
          message: invalidMessage,
        });
        return;
      }

      if (!columns?.length) {
        return;
      }

      const modifyTriggerState = { ...state };

      /* don't pass cleanup config if it's empty or just have only paused */
      if (
        JSON.stringify(modifyTriggerState?.cleanupConfig) === '{}' ||
        JSON.stringify(modifyTriggerState?.cleanupConfig) === '{"paused":true}'
      ) {
        delete modifyTriggerState?.cleanupConfig;
      }

      modifyEventTrigger(
        {
          state: modifyTriggerState,
          transformState,
          columns,
          source: currentSource,
          property,
          trigger: eventTrigger,
        },
        () => {
          if (successCb) {
            successCb();
          }
        },
        errorCb,
      );
    };

  const submit = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    saveWrapper()();
  };

  const deleteWrapper = () => {
    deleteEventTrigger(
      {
        name: eventTrigger.name,
        source: currentSource,
      },
      () => {
        navigate(dataRoutes.getDataEventsLandingRoute());
      },
    );
  };

  return (
    <Analytics name="ModifyEventTriggers" {...REDACT_EVERYTHING}>
      <div className="w-full overflow-y-auto">
        <div className="max-w-6xl">
          <form onSubmit={submit}>
            <Flex direction="column" gap="4">
              <TableHeader
                count={null}
                triggerName={eventTrigger.name}
                tabName="modify"
                readOnlyMode={readOnlyMode}
              />
              <div>
                <div className="mb-4">
                  <Heading as="h3" size="4">
                    Event Info
                  </Heading>
                </div>
                <Info
                  source={currentSource.name}
                  table={currentTable}
                  currentTrigger={eventTrigger}
                />
              </div>
              <WebhookEditor
                currentTrigger={eventTrigger}
                webhook={state.webhook}
                setWebhook={setState.webhook}
                save={saveWrapper('webhook')}
              />

              <OperationEditor
                currentTrigger={eventTrigger}
                table={currentTable}
                columns={columns}
                areColumnsFetching={areColumnsFetching}
                columnsFetchingError={columnsFetchingError}
                operations={state.operations}
                setOperations={setState.operations}
                operationColumns={state.operationColumns}
                setOperationColumns={setState.operationColumns}
                save={saveWrapper('ops')}
                isAllColumnChecked={state.isAllColumnChecked}
                toggleAllColumnChecked={setState.toggleAllColumnChecked}
                readOnlyMode={readOnlyMode}
              />
              <Separator size="4" />
              <RetryConfEditor
                conf={state.retryConf}
                setRetryConf={setState.retryConf}
                currentTrigger={eventTrigger}
                save={saveWrapper('retry_conf')}
              />
              <Separator size="4" />
              {autoCleanupSupport !== 'forbidden' && (
                <AutoCleanupForm
                  onChange={setState.cleanupConfig}
                  cleanupConfig={state?.cleanupConfig}
                />
              )}
              <HeadersEditor
                headers={state.headers}
                setHeaders={setState.headers}
                currentTrigger={eventTrigger}
                save={saveWrapper('headers')}
              />
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
              {!readOnlyMode && (
                <div>
                  <span className="mr-4">
                    <Button
                      mode="primary"
                      type="submit"
                      data-test="save-modify-trigger-changes"
                    >
                      Save Event Trigger
                    </Button>
                  </span>
                  <Button
                    mode="destructive"
                    data-test="delete-trigger"
                    onClick={deleteWrapper}
                  >
                    Delete Event Trigger
                  </Button>
                </div>
              )}
            </Flex>
          </form>
        </div>
      </div>
    </Analytics>
  );
};

export default ModifyEventTrigger;
