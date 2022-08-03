import React, { useEffect, useReducer } from 'react';
import { GraphQLError } from 'graphql';
import Helmet from 'react-helmet';
import { connect, ConnectedProps } from 'react-redux';
import Endpoints from '@/Endpoints';
import {
  parseValidateApiData,
  getValidateTransformOptions,
  getTransformState,
} from '@/components/Common/ConfigureTransformation/utils';
import requestAction from '@/utils/requestAction';
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
  setRequestTransformState,
} from '@/components/Common/ConfigureTransformation/requestTransformState';
import {
  KeyValuePair,
  RequestTransformStateBody,
} from '@/components/Common/ConfigureTransformation/stateDefaults';
import {
  RequestTransformContentType,
  RequestTransformMethod,
} from '@/metadata/types';
import ActionEditor from '../Common/components/ActionEditor';
import Button from '../../../Common/Button';
import ActionContainer from '../Containers/ActionContainer';
import { getModifyState } from './utils';
import {
  setModifyState,
  setActionHandler,
  setActionKind,
  setActionDefinition,
  setTypeDefinition,
  setActionComment,
  setHeaders as dispatchNewHeaders,
  toggleForwardClientHeaders as toggleFCH,
  setActionTimeout,
} from './reducer';
import { saveAction, deleteAction } from '../ServerIO';
import { getActionDefinitionFromSdl } from '../../../../shared/utils/sdlUtils';
import ConfigureTransformation from '../../../Common/ConfigureTransformation/ConfigureTransformation';
import {
  ActionExecution,
  defaultActionDefSdl,
  defaultTypesDefSdl,
  Header,
} from '../Common/stateDefaults';
import { Nullable } from '../../../Common/utils/tsUtils';
import { mapDispatchToPropsEmpty } from '../../../Common/utils/reactUtils';
import { ReduxState } from '../../../../types';
import {
  actionsSelector,
  customTypesSelector,
} from '../../../../metadata/selector';
import { getActionRequestSampleInput } from '../Add/utils';

const ModifyAction: React.FC<ModifyProps> = ({
  currentAction,
  actionName,
  allTypes,
  dispatch,
  isFetching,
  headers,
  forwardClientHeaders,
  readOnlyMode,
  ...modifyProps
}) => {
  const {
    handler,
    kind,
    actionDefinition,
    typeDefinition,
    comment,
    timeout,
  } = modifyProps;

  const {
    sdl: typeDefinitionSdl,
    error: typesDefinitionError,
    timer: typeDefinitionTimer,
  } = typeDefinition;

  const {
    sdl: actionDefinitionSdl,
    error: actionDefinitionError,
    timer: actionDefinitionTimer,
  } = actionDefinition;

  const [transformState, transformDispatch] = useReducer(
    requestTransformReducer,
    getActionRequestTransformDefaultState()
  );

  // initialize action state
  const init = () => {
    const modifyState = getModifyState(currentAction, allTypes);
    dispatch(setModifyState(modifyState));
    if (currentAction?.definition?.request_transform) {
      const requestSampleInput =
        !actionDefinitionError && !typesDefinitionError
          ? getActionRequestSampleInput(actionDefinitionSdl, typeDefinitionSdl)
          : getActionRequestSampleInput(
              defaultActionDefSdl,
              defaultTypesDefSdl
            );
      const rtState = getTransformState(
        currentAction?.definition?.request_transform,
        requestSampleInput
      );
      transformDispatch(setRequestTransformState(rtState));
    } else {
      transformDispatch(
        setRequestTransformState(getActionRequestTransformDefaultState())
      );
    }
  };
  useEffect(init, [currentAction, allTypes, dispatch]);

  const handlerOnChange = (val: string) => dispatch(setActionHandler(val));
  const executionOnChange = (k: ActionExecution) => dispatch(setActionKind(k));
  const timeoutOnChange = (e: React.ChangeEvent<HTMLInputElement>) =>
    dispatch(setActionTimeout(e.target.value));
  const commentOnChange = (e: React.ChangeEvent<HTMLInputElement>) =>
    dispatch(setActionComment(e.target.value));

  const actionDefinitionOnChange = (
    value: Nullable<string>,
    error: Nullable<GraphQLError>,
    timer: Nullable<NodeJS.Timeout>,
    ast: Nullable<Record<string, any>>
  ) => {
    dispatch(setActionDefinition(value, error, timer, ast));
  };

  const typeDefinitionOnChange = (
    value: Nullable<string>,
    error: Nullable<GraphQLError>,
    timer: Nullable<NodeJS.Timeout>,
    ast: Nullable<Record<string, any>>
  ) => {
    dispatch(setTypeDefinition(value, error as any, timer, ast));
  };

  const onSave = () => {
    dispatch(saveAction(currentAction, transformState));
  };

  const onDelete = () => {
    dispatch(deleteAction(currentAction));
  };

  const setHeaders = (hs: Header[]) => {
    dispatch(dispatchNewHeaders(hs));
  };

  const toggleForwardClientHeaders = () => {
    dispatch(toggleFCH());
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

  const requestQueryParamsOnChange = (requestQueryParams: KeyValuePair[]) => {
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
    webhookUrl: handler,
    envVarsFromContext: transformState.envVars,
    sessionVarsFromContext: transformState.sessionVars,
    transformerBody: transformState.requestBody,
  });
  useEffect(() => {
    requestBodyErrorOnChange('');
    requestTransformedBodyOnChange('');
    if (!handler) {
      requestBodyErrorOnChange(
        'Please configure your webhook handler to generate request body transform'
      );
      requestTransformedBodyOnChange('');
    } else if (transformState.requestBody && handler) {
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
    handler,
    transformState.envVars,
    transformState.sessionVars,
  ]);

  useEffect(() => {
    if (
      transformState.requestBody &&
      handler &&
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

  const allowSave =
    !isFetching &&
    !typesDefinitionError &&
    !actionDefinitionError &&
    !actionDefinitionTimer &&
    !typeDefinitionTimer;

  let actionType = '';
  if (!actionDefinitionError) {
    const { type, error } = getActionDefinitionFromSdl(actionDefinitionSdl);
    if (!error) {
      actionType = type;
    }
  }

  return (
    <>
      <Helmet>
        <title data-heap-redact-text="true">{`Modify Action - ${actionName} - Actions | Hasura`}</title>
      </Helmet>

      <div className="w-full overflow-y-auto bg-gray-50">
        <div className="max-w-6xl">
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

          <div className="flex items-start mb-lg">
            {!readOnlyMode && (
              <>
                <Button
                  color="yellow"
                  size="sm"
                  type="submit"
                  onClick={onSave}
                  disabled={!allowSave}
                  className="mb-5 mr-5"
                  data-test="save-modify-action-changes"
                >
                  Save Action
                </Button>
                <Button
                  color="red"
                  size="sm"
                  type="submit"
                  onClick={onDelete}
                  disabled={isFetching}
                  data-test="delete-action"
                >
                  Delete Action
                </Button>
              </>
            )}
          </div>
        </div>
      </div>
    </>
  );
};

interface ModifyProps extends InjectedProps {}

const Modify: React.FC<ModifyProps> = ({
  params,
  allActions,
  allTypes,
  dispatch,
  ...modifyProps
}) => (
  <ActionContainer
    params={params}
    allActions={allActions}
    tabName="modify"
    dispatch={dispatch}
  >
    <ModifyAction
      allActions={allActions}
      allTypes={allTypes}
      dispatch={dispatch}
      actionName={params.actionName}
      {...modifyProps}
    />
  </ActionContainer>
);

const mapStateToProps = (state: ReduxState) => {
  return {
    ...state.actions.modify,
    allActions: actionsSelector(state),
    allTypes: customTypesSelector(state),
    readOnlyMode: state.main.readOnlyMode,
  };
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);

type InjectedProps = ConnectedProps<typeof connector>;

const ModifyActionConnector = connector(Modify);

export default ModifyActionConnector;
