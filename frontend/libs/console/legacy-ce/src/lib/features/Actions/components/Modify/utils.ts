import {
  getActionDefinitionSdl,
  getTypesSdl,
} from '../../../../shared/utils/sdlUtils';
import { parseServerHeaders } from '@hasura/shared/ui';
import {
  getActionArguments,
  getActionOutputType,
  getActionType,
} from '../../utils';
import type { Action } from '@hasura/shared/types';
import type { FlattenCustomType } from '../../../../shared/utils/hasuraCustomTypeUtils';
import type { ActionState } from '../../types';
import { getActionRequestSampleInput, getActionTypes } from '../Form/utils';
import {
  defaultActionDefSdl,
  defaultTypesDefSdl,
  getActionRequestTransformDefaultState,
  getActionResponseTransformDefaultState,
} from '../Form/state';
import {
  getResponseTransformState,
  getTransformState,
} from '../../../ConfigureTransformation/utils';

export const getModifyState = (
  currentAction: Action,
  allTypes: FlattenCustomType[],
): ActionState => {
  const { definition: actionDef } = currentAction;
  const actionSdl = getActionDefinitionSdl(
    currentAction.name,
    getActionType(currentAction),
    getActionArguments(currentAction),
    getActionOutputType(currentAction),
    currentAction.comment,
  );

  return {
    actionDefinition: {
      sdl: actionSdl,
      error: null,
    },
    typeDefinition: {
      sdl: getTypesSdl(getActionTypes(currentAction, allTypes)),
      error: null,
    },
    handler: actionDef.handler,
    kind: actionDef.kind ?? 'synchronous',
    headers: parseServerHeaders(actionDef.headers),
    forwardClientHeaders: actionDef.forward_client_headers,
    timeout: actionDef?.timeout ? actionDef.timeout.toString() : '',
    comment: currentAction.comment ?? '',
  };
};

export const getActionRequestTransformState = (
  action: Action,
  state: ActionState,
) => {
  if (!action.definition?.request_transform) {
    return getActionRequestTransformDefaultState();
  }

  const requestSampleInput = state.actionDefinition.sdl
    ? getActionRequestSampleInput(
        state.actionDefinition.sdl,
        state.typeDefinition.sdl,
      )
    : getActionRequestSampleInput(defaultActionDefSdl, defaultTypesDefSdl);

  return getTransformState(
    action?.definition?.request_transform,
    JSON.stringify(requestSampleInput),
  );
};

export const getActionResponseTransformState = (action: Action) => {
  if (!action.definition?.response_transform) {
    return getActionResponseTransformDefaultState();
  }

  return getResponseTransformState(action.definition.response_transform);
};
