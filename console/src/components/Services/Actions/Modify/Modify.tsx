import React, { useEffect, useReducer } from 'react';
import { GraphQLError } from 'graphql';
import Helmet from 'react-helmet';
import { connect, ConnectedProps } from 'react-redux';
import Endpoints from '@/Endpoints';
import {
  parseValidateApiData,
  getValidateTransformOptions,
} from '@/components/Common/ConfigureTransformation/utils';
import requestAction from '@/utils/requestAction';
import {
  getActionRequestTransformDefaultState,
  requestTransformReducer,
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
import { KeyValuePair } from '@/components/Common/ConfigureTransformation/stateDefaults';
import {
  RequestTransformContentType,
  RequestTransformMethod,
} from '@/metadata/types';
import styles from '../../../Common/Common.scss';
import ActionEditor from '../Common/components/ActionEditor';
import Button from '../../../Common/Button';
import ActionContainer from '../Containers/ActionContainer';
import { getModifyState, getTransformState } from './utils';
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
    const rtState =
      !actionDefinitionError && !typesDefinitionError
        ? getTransformState(
            currentAction,
            actionDefinitionSdl,
            typeDefinitionSdl
          )
        : getTransformState(
            currentAction,
            defaultActionDefSdl,
            defaultTypesDefSdl
          );
    transformDispatch(setRequestTransformState(rtState));
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
    const onResponse = (data: Record<string, any>) => {
      parseValidateApiData(
        data,
        requestUrlErrorOnChange,
        requestUrlPreviewOnChange
      );
    };
    const options = getValidateTransformOptions(
      transformState.requestSampleInput,
      handler,
      undefined,
      transformState.requestUrl,
      transformState.requestQueryParams
    );
    if (!handler) {
      requestUrlErrorOnChange(
        'Please configure your webhook handler to generate request url transform'
      );
      requestUrlPreviewOnChange('');
    } else if (!transformState.requestUrl) {
      requestUrlPreviewOnChange('');
    } else {
      requestUrlErrorOnChange('');
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
    handler,
    transformState.requestBody
  );
  useEffect(() => {
    if (!transformState.requestBody) {
      requestTransformedBodyOnChange('');
    } else if (transformState.requestBody && handler) {
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
  }, [transformState.requestSampleInput, transformState.requestBody]);

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
      <Helmet title={`Modify Action - ${actionName} - Actions | Hasura`} />

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
            webhookUrl={handler}
            state={transformState}
            resetSampleInput={resetSampleInput}
            requestMethodOnChange={requestMethodOnChange}
            requestUrlOnChange={requestUrlOnChange}
            requestUrlErrorOnChange={requestUrlErrorOnChange}
            requestQueryParamsOnChange={requestQueryParamsOnChange}
            requestAddHeadersOnChange={requestAddHeadersOnChange}
            requestBodyOnChange={requestBodyOnChange}
            requestBodyErrorOnChange={requestBodyErrorOnChange}
            requestSampleInputOnChange={requestSampleInputOnChange}
            requestContentTypeOnChange={requestContentTypeOnChange}
            requestUrlTransformOnChange={requestUrlTransformOnChange}
            requestPayloadTransformOnChange={requestPayloadTransformOnChange}
          />

          <div className="flex items-center mb-lg">
            {!readOnlyMode && (
              <React.Fragment>
                <Button
                  color="yellow"
                  size="sm"
                  type="submit"
                  onClick={onSave}
                  disabled={!allowSave}
                  className={styles.add_mar_right}
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
              </React.Fragment>
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
