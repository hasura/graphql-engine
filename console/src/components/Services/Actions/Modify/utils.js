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
import { getActionTypes } from '../Common/utils';

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
