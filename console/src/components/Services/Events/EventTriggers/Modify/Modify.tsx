import React, { useEffect, useReducer } from 'react';
import { connect, ConnectedProps } from 'react-redux';
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
} from '@/components/Common/ConfigureTransformation/requestTransformState';
import {
  RequestTransformContentType,
  RequestTransformMethod,
} from '@/metadata/types';
import { KeyValuePair } from '@/components/Common/ConfigureTransformation/stateDefaults';
import ConfigureTransformation from '@/components/Common/ConfigureTransformation/ConfigureTransformation';
import requestAction from '@/utils/requestAction';
import Endpoints from '@/Endpoints';
import {
  getValidateTransformOptions,
  parseValidateApiData,
  getTransformState,
} from '@/components/Common/ConfigureTransformation/utils';
import { Button } from '@/new-components/Button';
import { mapDispatchToPropsEmpty } from '../../../../Common/utils/reactUtils';
import { getEventRequestSampleInput } from '../utils';
import TableHeader from '../TableCommon/TableHeader';
import styles from './ModifyEvent.scss';
import { updateSchemaInfo } from '../../../Data/DataActions';
import { useEventTriggerModify } from '../state';
import Info from './Info';
import WebhookEditor from './WebhookEditor';
import { OperationEditor } from './OperationEditor';
import RetryConfEditor from './RetryConfEditor';
import HeadersEditor from './HeadersEditor';
import { ReduxState } from '../../../../../types';
import { RouterTriggerProps } from '../../types';
import { findETTable } from '../../utils';
import { EventTriggerProperty } from './utils';
import { modifyEventTrigger, deleteEventTrigger } from '../../ServerIO';
import { NotFoundError } from '../../../../Error/PageNotFound';
import { getEventTriggerByName } from '../../../../../metadata/selector';

interface Props extends InjectedProps {}

const Modify: React.FC<Props> = props => {
  const {
    currentTrigger,
    allSchemas,
    readOnlyMode,
    dispatch,
    currentDataSource,
  } = props;
  if (!currentTrigger) {
    // throw a 404 exception
    throw new NotFoundError();
  }

  const { state, setState } = useEventTriggerModify(
    currentTrigger,
    allSchemas,
    currentDataSource
  );

  const [transformState, transformDispatch] = useReducer(
    requestTransformReducer,
    getEventRequestTransformDefaultState()
  );

  useEffect(() => {
    if (currentTrigger) {
      dispatch(
        updateSchemaInfo({
          schemas: [currentTrigger.schema_name],
        })
      );
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

  const requestQueryParamsOnChange = (requestQueryParams: KeyValuePair[]) => {
    transformDispatch(setRequestQueryParams(requestQueryParams));
  };

  const requestAddHeadersOnChange = (requestAddHeaders: KeyValuePair[]) => {
    transformDispatch(setRequestAddHeaders(requestAddHeaders));
  };

  const requestBodyOnChange = (requestBody: string) => {
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
    const options = getValidateTransformOptions(
      transformState.requestSampleInput,
      state.webhook?.value,
      transformState.envVars,
      transformState.sessionVars,
      undefined,
      transformState.requestUrl,
      transformState.requestQueryParams,
      state.webhook.type === 'env'
    );
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
  const reqBodyoptions = getValidateTransformOptions(
    transformState.requestSampleInput,
    state.webhook?.value,
    transformState.envVars,
    transformState.sessionVars,
    transformState.requestBody,
    undefined,
    undefined,
    state.webhook.type === 'env'
  );
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

  useEffect(() => {
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
  }, [transformState.requestTransformedBody]);

  const table = findETTable(currentTrigger, allSchemas);

  const saveWrapper = (property?: EventTriggerProperty) => (
    successCb?: () => void,
    errorCb?: () => void
  ) => {
    dispatch(
      modifyEventTrigger(
        state,
        transformState,
        currentTrigger,
        property,
        table,
        successCb,
        errorCb
      )
    );
  };

  const deleteWrapper = () => {
    dispatch(deleteEventTrigger(currentTrigger));
  };

  return (
    <div className="w-full overflow-y-auto bg-gray-50">
      <div className="max-w-6xl">
        <TableHeader
          count={null}
          triggerName={currentTrigger.name}
          tabName="modify"
          readOnlyMode={readOnlyMode}
        />
        <br />
        <h2 className="text-lg font-semibold mb-xs flex items-center">
          Event Info
        </h2>
        <Info currentTrigger={currentTrigger} />
        <div className={styles.container}>
          <WebhookEditor
            currentTrigger={currentTrigger}
            webhook={state.webhook}
            setWebhook={setState.webhook}
            save={saveWrapper('webhook')}
            styles={styles}
          />
          <OperationEditor
            currentTrigger={currentTrigger}
            allTableColumns={
              findETTable(currentTrigger, allSchemas)?.columns || []
            }
            operations={state.operations}
            setOperations={setState.operations}
            operationColumns={state.operationColumns}
            setOperationColumns={setState.operationColumns}
            styles={styles}
            save={saveWrapper('ops')}
            isAllColumnChecked={state.isAllColumnChecked}
            handleColumnRadioButton={setState.toggleAllColumnChecked}
          />
          <RetryConfEditor
            conf={state.retryConf}
            setRetryConf={setState.retryConf}
            currentTrigger={currentTrigger}
            styles={styles}
            save={saveWrapper('retry_conf')}
          />
          <HeadersEditor
            headers={state.headers}
            setHeaders={setState.headers}
            styles={styles}
            currentTrigger={currentTrigger}
            save={saveWrapper('headers')}
          />
          <ConfigureTransformation
            state={transformState}
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
            requestPayloadTransformOnChange={requestPayloadTransformOnChange}
          />
          {!readOnlyMode && (
            <div className="mb-md">
              <span className="mr-md">
                <Button
                  mode="primary"
                  type="submit"
                  onClick={() => {
                    saveWrapper()();
                  }}
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
      </div>
    </div>
  );
};

const mapStateToProps = (state: ReduxState, ownProps: RouterTriggerProps) => {
  const modifyTriggerName = ownProps.params.triggerName;
  const currentTrigger = getEventTriggerByName(state)(modifyTriggerName);

  return {
    currentTrigger,
    allSchemas: state.tables.allSchemas,
    readOnlyMode: state.main.readOnlyMode,
    currentDataSource: state.tables.currentDataSource,
  };
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);
type InjectedProps = ConnectedProps<typeof connector>;

const ModifyConnector = connector(Modify);
export default ModifyConnector;
