import {
  getActionDefinitionSdl,
  getTypesSdl,
} from '../../../../shared/utils/sdlUtils';
import {
  getActionArguments,
  getActionName,
  getActionOutputType,
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
        getActionOutputType(currentAction)
      ),
      error: null,
    },
    typeDefinition: {
      sdl: getTypesSdl(getActionTypes(currentAction, allTypes)),
      error: null,
    },
    webhook: actionDef.webhook,
    kind: actionDef.kind,
  };
  return modifyState;
};
