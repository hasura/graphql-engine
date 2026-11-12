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
import { useNavigate, useSearchParams } from 'react-router';
import { useQueryClient } from '@tanstack/react-query';
import { METADATA_QUERY_KEY } from '@hasura/metadata/api';
import { dataRoutes } from '@hasura/shared/utils';
import { useCurrentActionContext } from '../../context';
import { Heading } from '@radix-ui/themes';

const AddAction: React.FC = () => {
  useDocumentTitle('Add Action - Actions | Hasura');
  const { createAction, isPending: isCreating } = useCreateAction();
  const { metadata } = useCurrentActionContext();
  const [searchParams] = useSearchParams();
  const navigate = useNavigate();
  const queryClient = useQueryClient();
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
      searchParams.get('derived_operation'),
    ),
    getActionRequestTransformDefaultState(),
    getActionResponseTransformDefaultState(),
  );

  const onSubmit = () => {
    const requestTransform = getRequestTransformObject(transformState);
    const responseTransform = getResponseTransformObject(
      responseTransformState,
    );
    createAction(
      {
        metadata,
        rawState: state,
        requestTransform,
        responseTransform,
      },
      async (actionName) => {
        // the edit page reads the action from the metadata, so wait for the
        // metadata to include it before navigating there
        await queryClient.refetchQueries({ queryKey: [METADATA_QUERY_KEY] });
        navigate(dataRoutes.manageAction(actionName, 'modify'));
      },
    ).catch(() => {
      //
    });
  };

  return (
    <Analytics name="AddAction" {...REDACT_EVERYTHING}>
      <div className="w-full overflow-y-auto">
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
            disabled={isCreating}
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
            disabled={isCreating}
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
                loading={isCreating}
                loadingText="Creating..."
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
