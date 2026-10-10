import React from 'react';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { Button, IconTooltip } from '@hasura/shared/ui';
import {
  getRequestTransformObject,
  getResponseTransformObject,
} from '../../../ConfigureTransformation/utils';
import ConfigureTransformation from '../../../ConfigureTransformation/ConfigureTransformation';
import ActionEditor from '../Common/ActionEditor';
import { useDocumentTitle } from '@hasura/shared/hooks';
import useCreateAction from '../../hooks/useCreateAction';
import getDefaultState, {
  getActionRequestTransformDefaultState,
  getActionResponseTransformDefaultState,
} from '../Form/state';
import useActionForm from '../Form/useActionForm';
import { useSearchParams } from 'react-router';
import { useCurrentActionContext } from '../../context';
import { Heading } from '@radix-ui/themes';

const AddAction: React.FC = () => {
  useDocumentTitle('Add Action - Actions | Hasura');
  const createAction = useCreateAction();
  const { metadata } = useCurrentActionContext();
  const [searchParams] = useSearchParams();
  const {
    state,
    actionDefinitionOnChange,
    commentOnChange,
    envVarsOnChange,
    executionOnChange,
    handlerOnChange,
    timeoutOnChange,
    transformState,
    responseTransformState,
    requestAddHeadersOnChange,
    requestBodyOnChange,
    requestContentTypeOnChange,
    requestMethodOnChange,
    requestPayloadTransformOnChange,
    requestQueryParamsOnChange,
    requestSampleInputOnChange,
    requestUrlOnChange,
    requestUrlTransformOnChange,
    resetSampleInput,
    responseBodyOnChange,
    responsePayloadTransformOnChange,
    sessionVarsOnChange,
    setHeaders,
    toggleForwardClientHeaders,
    typeDefinitionOnChange,
    actionType,
    allowSave,
    readOnlyMode,
  } = useActionForm(
    getDefaultState(
      searchParams.get('action_sdl'),
      searchParams.get('types_sdl'),
    ),
    getActionRequestTransformDefaultState(),
    getActionResponseTransformDefaultState(),
  );

  const onSubmit = () => {
    const requestTransform = getRequestTransformObject(transformState);
    const responseTransform = getResponseTransformObject(
      responseTransformState,
    );
    createAction({
      metadata,
      rawState: state,
      requestTransform,
      responseTransform,
    });
  };

  return (
    <Analytics name="AddAction" {...REDACT_EVERYTHING}>
      <div className="w-full overflow-y-auto mt-6 pr-4">
        <div>
          <div className="mb-4">
            <Heading size="6">Add a new action</Heading>
          </div>

          <ActionEditor
            {...state}
            readOnlyMode={readOnlyMode}
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
            requestTransformState={transformState}
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

          <div>
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

export default AddAction;
