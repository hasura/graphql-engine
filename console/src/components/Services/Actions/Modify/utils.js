import { getActionDefinitionSdl } from '../../Types/sdlUtils';

export const getModifyState = currentAction => {
  const { action_defn: actionDef } = currentAction;
  const modifyState = {
    name: currentAction.action_name,
    actionDefinition: {
      sdl: getActionDefinitionSdl(
        currentAction.action_name,
        actionDef.arguments,
        actionDef.output_type
      ),
      error: null,
    },
    typeDefinition: {
      sdl: '',
      error: null,
    },
    webhook: actionDef.webhook,
    kind: actionDef.kind,
  };
  return modifyState;
};
