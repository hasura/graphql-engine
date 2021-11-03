import {
  getActionDefinitionSdl,
  getTypesSdl,
} from '../../../../shared/utils/sdlUtils';
import { parseServerHeaders } from '../../../Common/Headers/utils';
import {
  getActionArguments,
  getActionName,
  getActionOutputType,
  getActionComment,
  getActionType,
} from '../utils';
import { defaultRequestContentType } from '../../../Common/ConfigureTransformation/stateDefaults';
import { getActionTypes } from '../Common/utils';
import { getArrayFromServerPairObject } from '../../../Common/ConfigureTransformation/utils';
import { getActionRequestSampleInput } from '../Add/utils';

export const getModifyState = (currentAction, allTypes) => {
  const { definition: actionDef } = currentAction;
  const modifyState = {
    name: currentAction.name,
    actionDefinition: {
      sdl: getActionDefinitionSdl(
        getActionName(currentAction),
        getActionType(currentAction),
        getActionArguments(currentAction),
        getActionOutputType(currentAction),
        getActionComment(currentAction)
      ),
      error: null,
    },
    typeDefinition: {
      sdl: getTypesSdl(getActionTypes(currentAction, allTypes)),
      error: null,
    },
    handler: actionDef.handler,
    kind: actionDef.kind,
    headers: parseServerHeaders(actionDef.headers),
    forwardClientHeaders: actionDef.forward_client_headers,
    timeout: actionDef?.timeout ?? '',
    comment: currentAction.comment,
  };
  return modifyState;
};

export const getTransformState = (currentAction, actionSdl, typesSdl) => {
  const transform = currentAction?.definition?.request_transform;
  const transformState = {
    requestMethod: transform?.method ?? null,
    requestUrl: transform?.url ?? '',
    requestUrlError: '',
    requestUrlPreview: '',
    requestQueryParams: getArrayFromServerPairObject(
      transform?.query_params
    ) ?? [{ name: '', value: '' }],
    requestAddHeaders: getArrayFromServerPairObject(
      transform?.request_headers?.add_headers
    ) ?? [{ name: '', value: '' }],
    requestBody: transform?.body ?? '',
    requestBodyError: '',
    requestSampleInput: getActionRequestSampleInput(actionSdl, typesSdl),
    requestTransformedBody: '',
    requestContentType: transform?.content_type ?? defaultRequestContentType,
    isRequestUrlTransform: !!transform?.method || !!transform?.url,
    isRequestPayloadTransform: !!transform?.body,
    templatingEngine: transform?.template_engine ?? 'Kriti',
  };
  return transformState;
};
