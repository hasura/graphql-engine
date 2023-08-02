import React, { useEffect, useReducer } from 'react';
import { GraphQLError } from 'graphql';
import { connect, ConnectedProps } from 'react-redux';
import Helmet from 'react-helmet';
import { Analytics, REDACT_EVERYTHING } from '../../../../features/Analytics';
import { IconTooltip } from '../../../../new-components/Tooltip';
import requestAction from '../../../../utils/requestAction';
import { Button } from '../../../../new-components/Button';
import {
  parseValidateApiData,
  getValidateTransformOptions,
} from '../../../Common/ConfigureTransformation/utils';
import Endpoints from '../../../../Endpoints';
import {
  getActionRequestTransformDefaultState,
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
  setResponsePayloadTransform,
  setResponseBody,
  responseTransformReducer,
  getActionResponseTransformDefaultState,
} from '../../../Common/ConfigureTransformation/requestTransformState';
import {
  QueryParams,
  RequestTransformContentType,
  RequestTransformMethod,
} from '../../../../metadata/types';
import {
  KeyValuePair,
  RequestTransformStateBody,
  ResponseTransformStateBody,
} from '../../../Common/ConfigureTransformation/stateDefaults';
import ConfigureTransformation from '../../../Common/ConfigureTransformation/ConfigureTransformation';
import ActionEditor from '../Common/components/ActionEditor';
import { createAction } from '../ServerIO';
import { getActionDefinitionFromSdl } from '../../../../shared/utils/sdlUtils';
import { showWarningNotification } from '../../Common/Notification';
import {
  setActionHandler,
  setActionExecution,
  setDefaults,
  setActionDefinition,
  setTypeDefinition,
  setHeaders as dispatchNewHeaders,
  toggleForwardClientHeaders as toggleFCH,
  resetDerivedActionParentOperation,
  setActionTimeout,
  setActionComment,
} from './reducer';
import { getActionRequestSampleInput } from './utils';
import { Header, ActionExecution } from '../Common/stateDefaults';
import { Nullable } from '../../../Common/utils/tsUtils';
import { ReduxState } from '../../../../types';
import { mapDispatchToPropsEmpty } from '../../../Common/utils/reactUtils';

type AddActionProps = InjectedProps;

const AddAction: React.FC<AddActionProps> = ({
  handler,
  dispatch,
  kind,
  actionDefinition,
  typeDefinition,
  isFetching,
  headers,
  forwardClientHeaders,
  derive,
  readOnlyMode,
  timeout,
  comment,
}) => {
  const [transformState, transformDispatch] = useReducer(
    requestTransformReducer,
    getActionRequestTransformDefaultState()
  );

  const [responseTransformState, responseTransformDispatch] = useReducer(
    responseTransformReducer,
    getActionResponseTransformDefaultState()
  );

  const createActionRef = React.useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (readOnlyMode)
      dispatch(
        showWarningNotification(
          'Failed to add action',
          'Adding new action is not allowed in Read only mode!'
        )
      );
  }, [dispatch, readOnlyMode]);
  useEffect(() => {
    if (!derive.operation) {
      dispatch(setDefaults());
    }
    return () => {
      dispatch(resetDerivedActionParentOperation());
    };
  }, []);

  const handlerOnChange = (val: string) => dispatch(setActionHandler(val));
  const executionOnChange = (k: ActionExecution) =>
    dispatch(setActionExecution(k));
  const timeoutOnChange = (e: React.ChangeEvent<HTMLInputElement>) =>
    dispatch(setActionTimeout(e.target.value));
  const commentOnChange = (e: React.ChangeEvent<HTMLInputElement>) =>
    dispatch(setActionComment(e.target.value));
  const {
    sdl: typeDefinitionSdl,
    error: typesDefinitionError,
    timer: typedefParseTimer,
  } = typeDefinition;

  const {
    sdl: actionDefinitionSdl,
    error: actionDefinitionError,
    timer: actionParseTimer,
  } = actionDefinition;

  const onSubmit = () => {
    dispatch(createAction(transformState, responseTransformState));
  };

  const setHeaders = (hs: Header[]) => {
    dispatch(dispatchNewHeaders(hs));
  };

  const toggleForwardClientHeaders = () => {
    dispatch(toggleFCH());
  };

  const actionDefinitionOnChange = (
    value: Nullable<string>,
    error: Nullable<GraphQLError>,
    timer: Nullable<NodeJS.Timeout>,
    ast: Nullable<Record<string, any>>
  ) => {
    dispatch(setActionDefinition(value as string, error, timer, ast));
  };

  const typeDefinitionOnChange = (
    value: Nullable<string>,
    error: Nullable<GraphQLError>,
    timer: Nullable<NodeJS.Timeout>,
    ast: Nullable<Record<string, any>>
  ) => {
    dispatch(setTypeDefinition(value as string, error, timer, ast));
  };

  // request transform methods
  const resetSampleInput = () => {
    if (!actionDefinitionError && !typesDefinitionError) {
      const value = getActionRequestSampleInput(
        actionDefinitionSdl,
        typeDefinitionSdl
      );
      transformDispatch(setRequestSampleInput(value));
    }
  };

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

  const responsePayloadTransformOnChange = (data: boolean) => {
    responseTransformDispatch(setResponsePayloadTransform(data));
  };

  const responseBodyOnChange = (responseBody: ResponseTransformStateBody) => {
    responseTransformDispatch(setResponseBody(responseBody));
  };

  // we send separate requests for the `url` preview and `body` preview, as in case of error,
  // we will not be able to resolve if the error is with url or body transform, with the current state of `test_webhook_transform` api
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
      webhookUrl: handler,
      envVarsFromContext: transformState.envVars,
      sessionVarsFromContext: transformState.sessionVars,
      requestUrl: transformState.requestUrl,
      queryParams: transformState.requestQueryParams,
    });
    if (!handler) {
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
    handler,
    transformState.requestUrl,
    transformState.requestQueryParams,
    transformState.envVars,
    transformState.sessionVars,
  ]);

  useEffect(() => {
    requestBodyErrorOnChange('');
    requestTransformedBodyOnChange('');
    const onResponse = (data: Record<string, any>) => {
      parseValidateApiData(
        data,
        requestBodyErrorOnChange,
        undefined,
        requestTransformedBodyOnChange
      );
    };
    const options = getValidateTransformOptions({
      version: transformState.version,
      inputPayloadString: transformState.requestSampleInput,
      webhookUrl: handler,
      envVarsFromContext: transformState.envVars,
      sessionVarsFromContext: transformState.sessionVars,
      transformerBody: transformState.requestBody,
    });
    if (!handler) {
      requestBodyErrorOnChange(
        'Please configure your webhook handler to generate request body transform'
      );
    } else if (transformState.requestBody && handler) {
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
    transformState.requestBody,
    handler,
    transformState.envVars,
    transformState.sessionVars,
  ]);

  const allowSave =
    !isFetching &&
    !typesDefinitionError &&
    !actionDefinitionError &&
    !actionParseTimer &&
    !typedefParseTimer &&
    !readOnlyMode;

  let actionType = '';
  if (!actionDefinitionError && !actionParseTimer) {
    const { type, error } = getActionDefinitionFromSdl(actionDefinitionSdl);
    if (!error) {
      actionType = type;
    }
  }

  return (
    <Analytics name="AddAction" {...REDACT_EVERYTHING}>
      <div className="w-full overflow-y-auto bg-gray-50 bootstrap-jail">
        <div className="max-w-6xl">
          <Helmet title="Add Action - Actions | Hasura" />
          <h2 className="font-bold text-xl mb-5">Add a new action</h2>

          <ActionEditor
            handler={handler}
            execution={kind}
            actionDefinition={actionDefinition}
            typeDefinition={typeDefinition}
            headers={headers}
            forwardClientHeaders={forwardClientHeaders}
            readOnlyMode={readOnlyMode}
            timeout={timeout}
            comment={comment}
            actionType={actionType}
            commentOnChange={commentOnChange}
            handlerOnChange={handlerOnChange}
            executionOnChange={executionOnChange}
            timeoutOnChange={timeoutOnChange}
            setHeaders={setHeaders}
            toggleForwardClientHeaders={toggleForwardClientHeaders}
            actionDefinitionOnChange={actionDefinitionOnChange}
            typeDefinitionOnChange={typeDefinitionOnChange}
          />

          <ConfigureTransformation
            transformationType="action"
            requestTransfromState={transformState}
            responseTransformState={responseTransformState}
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
            responsePayloadTransformOnChange={responsePayloadTransformOnChange}
            responseBodyOnChange={responseBodyOnChange}
          />

          <div ref={createActionRef}>
            <Analytics
              name="actions-tab-create-action-button"
              passHtmlAttributesToChildren
            >
              <Button
                mode="primary"
                size="md"
                type="submit"
                disabled={!allowSave}
                onClick={onSubmit}
                data-test="create-action-btn"
              >
                Create Action
              </Button>
            </Analytics>
            {readOnlyMode && (
              <IconTooltip message="Adding new action is not allowed in Read only mode!" />
            )}
          </div>
        </div>
      </div>
    </Analytics>
  );
};

const mapStateToProps = (state: ReduxState) => {
  return {
    ...state.actions.add,
    readOnlyMode: state.main.readOnlyMode,
  };
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);

type InjectedProps = ConnectedProps<typeof connector>;

const AddActionConnector = connector(AddAction);

export default AddActionConnector;
