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
} from '../utils';
import { getActionTypes } from '../Common/utils';

export const getModifyState = (currentAction, allTypes) => {
  const { action_defn: actionDef } = currentAction;
  const modifyState = {
    name: currentAction.action_name,
    actionDefinition: {
      sdl: getActionDefinitionSdl(
        getActionName(currentAction),
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
  };
  return modifyState;
};
