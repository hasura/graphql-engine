/* eslint-disable no-underscore-dangle */
import React, { useEffect, useReducer } from 'react';
import { connect, ConnectedProps } from 'react-redux';
import {
  Analytics,
  REDACT_EVERYTHING,
} from '../../../../../features/Analytics';

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
  setRequestTransformState,
  setRequestPayloadTransform,
  getEventRequestTransformDefaultState,
} from '../../../../Common/ConfigureTransformation/requestTransformState';
import {
  QueryParams,
  RequestTransformContentType,
  RequestTransformMethod,
} from '../../../../../metadata/types';
import {
  KeyValuePair,
  RequestTransformStateBody,
} from '../../../../Common/ConfigureTransformation/stateDefaults';
import ConfigureTransformation from '../../../../Common/ConfigureTransformation/ConfigureTransformation';
import requestAction from '../../../../../utils/requestAction';
import Endpoints from '../../../../../Endpoints';
import { useEELiteAccess } from '../../../../../features/EETrial';
import {
  getValidateTransformOptions,
  parseValidateApiData,
  getTransformState,
} from '../../../../../components/Common/ConfigureTransformation/utils';
import { showErrorNotification } from '../../../../../components/Services/Common/Notification';
import { Button } from '../../../../../new-components/Button';
import { isProConsole } from '../../../../../utils/proConsole';
import globals from '../../../../../Globals';
import { getSourceDriver } from '../../../Data/utils';
import { mapDispatchToPropsEmpty } from '../../../../Common/utils/reactUtils';
import { getEventRequestSampleInput } from '../utils';
import TableHeader from '../TableCommon/TableHeader';
import { getDatabaseSchemasInfo } from '../../../Data/DataActions';
import { parseServerETDefinition, useEventTriggerModify } from '../state';
import Info from './Info';
import WebhookEditor from './WebhookEditor';
import { OperationEditor } from './OperationEditor';
import RetryConfEditor from './RetryConfEditor';
import HeadersEditor from './HeadersEditor';
import { ReduxState } from '../../../../../types';
import { DatabaseInfo, RouterTriggerProps } from '../../types';
import { EventTriggerProperty } from './utils';
import { modifyEventTrigger, deleteEventTrigger } from '../../ServerIO';
import { NotFoundError } from '../../../../Error/PageNotFound';
import {
  getDataSources,
  getEventTriggerByName,
} from '../../../../../metadata/selector';
import { AutoCleanupForm } from '../Common/AutoCleanupForm';
import { useDebouncedEffect } from '../../../../../hooks/useDebounceEffect';

type Props = InjectedProps;

const Modify: React.FC<Props> = props => {
  const { currentTrigger, readOnlyMode, dataSourcesList, dispatch } = props;
  if (!currentTrigger) {
    // throw a 404 exception
    throw new NotFoundError();
  }

  const [databaseInfo, setDatabaseInfo] = React.useState<DatabaseInfo>({});
  const { state, setState } = useEventTriggerModify(
    currentTrigger,
    databaseInfo
  );

  const [transformState, transformDispatch] = useReducer(
    requestTransformReducer,
    getEventRequestTransformDefaultState()
  );

  const { access: eeLiteAccess } = useEELiteAccess(globals);
  const autoCleanupSupport =
    isProConsole(globals) || eeLiteAccess === 'active'
      ? 'active'
      : eeLiteAccess;

  useEffect(() => {
    if (currentTrigger) {
      const driver = getSourceDriver(dataSourcesList, currentTrigger.source);
      if (currentTrigger.source) {
        dispatch(
          getDatabaseSchemasInfo(driver, currentTrigger.source) as any
        ).then((dbInfo: DatabaseInfo) => {
          setState.bulk(parseServerETDefinition(currentTrigger, dbInfo));
          setDatabaseInfo(dbInfo);
        });
      }
    }
  }, [currentTrigger.name]);

  useEffect(() => {
    if (currentTrigger.request_transform) {
      const sampleInput = getEventRequestSampleInput(
        currentTrigger.name,
        currentTrigger.table_name,
        currentTrigger.schema_name,
        currentTrigger.configuration?.retry_conf?.num_retries,
        state.operationColumns,
        state.operations,
        state.isAllColumnChecked
      );
      transformDispatch(
        setRequestTransformState(
          getTransformState(currentTrigger.request_transform, sampleInput)
        )
      );
    } else {
      transformDispatch(
        setRequestTransformState(getEventRequestTransformDefaultState())
      );
    }
  }, [state.name]);

  const resetSampleInput = () => {
    const value = getEventRequestSampleInput(
      currentTrigger.name,
      currentTrigger.table_name,
      currentTrigger.schema_name,
      currentTrigger.configuration.retry_conf.num_retries,
      state.operationColumns,
      state.operations,
      state.isAllColumnChecked
    );
    transformDispatch(setRequestSampleInput(value));
  };

  useEffect(() => {
    resetSampleInput();
  }, [state.webhook?.value]);

  const envVarsOnChange = (envVars: KeyValuePair[]) => {
    transformDispatch(setEnvVars(envVars));
  };

  const sessionVarsOnChange = (sessionVars: KeyValuePair[]) => {
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

  const requestAddHeadersOnChange = (requestAddHeaders: KeyValuePair[]) => {
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
    requestContentType: RequestTransformContentType
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
        requestUrlPreviewOnChange
      );
    };
    const options = getValidateTransformOptions({
      version: transformState.version,
      inputPayloadString: transformState.requestSampleInput,
      webhookUrl: state.webhook?.value,
      envVarsFromContext: transformState.envVars,
      sessionVarsFromContext: transformState.sessionVars,
      requestUrl: transformState.requestUrl,
      queryParams: transformState.requestQueryParams,
      isEnvVar: state.webhook.type === 'env',
    });
    if (!state.webhook?.value) {
      requestUrlErrorOnChange(
        'Please configure your webhook handler to generate request url transform'
      );
    } else {
      dispatch(
        requestAction(
          Endpoints.metadata,
          options,
          undefined,
          undefined,
          true,
          true
        )
      ).then(onResponse, onResponse); // parseValidateApiData will parse both success and error
    }
  }, [
    transformState.requestSampleInput,
    state.webhook?.value,
    transformState.requestUrl,
    transformState.requestQueryParams,
    transformState.envVars,
    transformState.sessionVars,
  ]);

  const onRequestBodyResponse = (data: Record<string, any>) => {
    parseValidateApiData(
      data,
      requestBodyErrorOnChange,
      undefined,
      requestTransformedBodyOnChange
    );
  };
  const reqBodyoptions = getValidateTransformOptions({
    version: transformState.version,
    inputPayloadString: transformState.requestSampleInput,
    webhookUrl: state.webhook?.value,
    envVarsFromContext: transformState.envVars,
    sessionVarsFromContext: transformState.sessionVars,
    transformerBody: transformState.requestBody,
    isEnvVar: state.webhook.type === 'env',
  });
  useEffect(() => {
    requestBodyErrorOnChange('');
    requestTransformedBodyOnChange('');
    if (!state.webhook?.value) {
      requestBodyErrorOnChange(
        'Please configure your webhook handler to generate request body transform'
      );
    } else if (transformState.requestBody && state.webhook?.value) {
      dispatch(
        requestAction(
          Endpoints.metadata,
          reqBodyoptions,
          undefined,
          undefined,
          true,
          true
        )
      ).then(onRequestBodyResponse, onRequestBodyResponse);
    }
  }, [
    transformState.requestSampleInput,
    transformState.requestBody,
    state.webhook?.value,
    transformState.envVars,
    transformState.sessionVars,
  ]);

  useDebouncedEffect(
    () => {
      if (
        transformState.requestBody &&
        state.webhook?.value &&
        !transformState.requestTransformedBody
      ) {
        requestBodyErrorOnChange('');
        dispatch(
          requestAction(
            Endpoints.metadata,
            reqBodyoptions,
            undefined,
            undefined,
            true,
            true
          )
        ).then(onRequestBodyResponse, onRequestBodyResponse);
      }
    },
    1000,
    [transformState.requestTransformedBody]
  );

  const saveWrapper =
    (property?: EventTriggerProperty) =>
    (successCb?: () => void, errorCb?: () => void) => {
      if (
        state.operationColumns.every(
          operationColumn => !operationColumn.enabled
        ) &&
        state.operations.update
      ) {
        dispatch(
          showErrorNotification(
            'Updating event trigger failed.',
            'Please select at-least one trigger column for the update trigger operation.'
          )
        );
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

      dispatch(
        modifyEventTrigger(
          modifyTriggerState,
          transformState,
          currentTrigger,
          databaseInfo,
          property,
          successCb,
          errorCb
        )
      );
    };

  const submit = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    saveWrapper()();
  };

  const deleteWrapper = () => {
    dispatch(deleteEventTrigger(currentTrigger));
  };

  return (
    <Analytics name="ModifyEventTriggers" {...REDACT_EVERYTHING}>
      <div className="w-full overflow-y-auto bg-gray-50">
        <div className="max-w-6xl">
          <form onSubmit={submit}>
            <TableHeader
              count={null}
              triggerName={currentTrigger.name}
              tabName="modify"
              readOnlyMode={readOnlyMode}
            />
            <br />
            <div className="bootstrap-jail">
              <h2 className="text-lg font-semibold mb-xs flex items-center">
                Event Info
              </h2>
              <Info currentTrigger={currentTrigger} />
            </div>
            <div className="relative bootstrap-jail">
              <WebhookEditor
                currentTrigger={currentTrigger}
                webhook={state.webhook}
                setWebhook={setState.webhook}
                save={saveWrapper('webhook')}
              />
              <OperationEditor
                currentTrigger={currentTrigger}
                databaseInfo={databaseInfo}
                operations={state.operations}
                setOperations={setState.operations}
                operationColumns={state.operationColumns}
                setOperationColumns={setState.operationColumns}
                save={saveWrapper('ops')}
                isAllColumnChecked={state.isAllColumnChecked}
                handleColumnRadioButton={setState.toggleAllColumnChecked}
              />
              <hr className="my-md" />
              <RetryConfEditor
                conf={state.retryConf}
                setRetryConf={setState.retryConf}
                currentTrigger={currentTrigger}
                save={saveWrapper('retry_conf')}
              />
              <hr className="my-md" />
              {autoCleanupSupport !== 'forbidden' && (
                <div className="mb-md">
                  <AutoCleanupForm
                    onChange={setState.cleanupConfig}
                    cleanupConfig={state?.cleanupConfig}
                  />
                </div>
              )}
              <HeadersEditor
                headers={state.headers}
                setHeaders={setState.headers}
                currentTrigger={currentTrigger}
                save={saveWrapper('headers')}
              />
              <ConfigureTransformation
                transformationType="event"
                requestTransfromState={transformState}
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
                <div className="mb-md">
                  <span className="mr-md">
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
            </div>
          </form>
        </div>
      </div>
    </Analytics>
  );
};

const mapStateToProps = (state: ReduxState, ownProps: RouterTriggerProps) => {
  const modifyTriggerName = ownProps.params.triggerName;
  const currentTrigger = getEventTriggerByName(state)(modifyTriggerName);
  const dataSourcesList = getDataSources(state);

  return {
    currentTrigger,
    readOnlyMode: state.main.readOnlyMode,
    dataSourcesList,
  };
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);
type InjectedProps = ConnectedProps<typeof connector>;

const ModifyConnector = connector(Modify);
export default ModifyConnector;
