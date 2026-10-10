import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { useDocumentTitle } from '@hasura/shared/hooks';
import {
  getRequestTransformObject,
  getResponseTransformObject,
} from '../../../ConfigureTransformation/utils';
import { Button } from '@hasura/shared/ui';
import {
  getActionRequestTransformState,
  getActionResponseTransformState,
} from './utils';
import ConfigureTransformation from '../../../ConfigureTransformation/ConfigureTransformation';
import useDeleteAction from '../../hooks/useDeleteAction';
import useSaveAction from '../../hooks/useSaveAction';
import ActionEditor from '../Common/ActionEditor';
import { useCurrentActionContext } from '../../context';
import useActionForm from '../Form/useActionForm';
import { ActionState } from '../../types';
import { Flex } from '@radix-ui/themes';

type Props = {
  initialState: ActionState;
};

const ModifyActionForm = ({ initialState }: Props) => {
  const deleteAction = useDeleteAction();
  const saveAction = useSaveAction();
  const { currentAction, metadata } = useCurrentActionContext();

  useDocumentTitle(`Modify Action - ${currentAction.name} - Actions | Hasura`);

  const {
    state,
    isFetching,
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
    initialState,
    getActionRequestTransformState(currentAction, initialState),
    getActionResponseTransformState(currentAction),
  );

  const onSave = () => {
    const requestTransform = getRequestTransformObject(transformState);
    const responseTransform = getResponseTransformObject(
      responseTransformState,
    );

    saveAction({
      metadata,
      currentAction,
      rawState: state,
      requestTransform,
      responseTransform,
    });
  };

  const onDelete = () => {
    deleteAction(currentAction.name);
  };

  return (
    <Analytics name="ModifyAction" {...REDACT_EVERYTHING}>
      <div className="w-full overflow-y-auto">
        <div className="max-w-6xl">
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

          <Flex align="start" className="mb-6">
            {!readOnlyMode && (
              <>
                <div className="mr-5">
                  <Button
                    mode="primary"
                    onClick={onSave}
                    disabled={!allowSave}
                    data-test="save-modify-action-changes"
                  >
                    Save Action
                  </Button>
                </div>
                <Button
                  mode="destructive"
                  size="md"
                  onClick={onDelete}
                  disabled={isFetching}
                  data-test="delete-action"
                >
                  Delete Action
                </Button>
              </>
            )}
          </Flex>
        </div>
      </div>
    </Analytics>
  );
};

export default ModifyActionForm;
