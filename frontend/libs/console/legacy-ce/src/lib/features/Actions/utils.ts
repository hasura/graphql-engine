import type { CustomTypes, Action, InputArgument } from '@hasura/shared/types';
import { FlattenCustomType } from '../../shared/utils/hasuraCustomTypeUtils';
import { setLSItem, getParsedLSItem } from '@hasura/shared/utils';
import { LS_KEYS } from '@hasura/shared/types';

export const findType = (types: FlattenCustomType[], typeName: string) => {
  return types.find((t) => t.definition.name === typeName);
};

export const findAction = (
  actions: Action[] | undefined,
  actionName: string,
) => {
  return actions?.find((a) => a.name === actionName);
};

export const getActionOutputType = (action: Action) => {
  return action.definition.output_type;
};

export const getActionOutputFields = (
  action: Action,
  types: FlattenCustomType[],
) => {
  const outputTypeName = getActionOutputType(action);

  const outputType = findType(types, outputTypeName);

  if (outputType?.kind === 'objects') {
    return outputType.definition.fields;
  }

  return null;
};

export const getActionArguments = (action: Action): InputArgument[] => {
  return action.definition.arguments || [];
};

export const getActionType = (action: Action) => {
  return action.definition.type ?? 'query';
};

export const getActionPermissions = (action: Action) => {
  return action.permissions;
};

export const persistAllDerivedActions = (allActions) => {
  let stringified;
  try {
    stringified = JSON.stringify(allActions);
  } catch (e) {
    stringified = '{}';
  }
  setLSItem(LS_KEYS.derivedActions, stringified);
};

export const getAllPersistedDerivedActions = () => {
  return getParsedLSItem(LS_KEYS.derivedActions, {});
};

export const getPersistedDerivedAction = (actionName) => {
  return getAllPersistedDerivedActions()[actionName];
};

export const persistDerivedAction = (actionName, parentOperation) => {
  const allActions = getAllPersistedDerivedActions();
  allActions[actionName] = parentOperation;
  persistAllDerivedActions(allActions);
};

export const removePersistedDerivedAction = (actionName) => {
  const allActions = getAllPersistedDerivedActions();
  delete allActions[actionName];
  persistAllDerivedActions(allActions);
};

export const updatePersistedDerivation = (oldActionName, newActionName) => {
  const parentOperation = getPersistedDerivedAction(oldActionName);
  if (parentOperation) {
    persistDerivedAction(newActionName, parentOperation);
    removePersistedDerivedAction(oldActionName);
  }
};

export const removeTypeRelationship = (
  types: CustomTypes,
  typename: string,
  relName: string,
): CustomTypes => {
  return {
    ...types,
    objects: types.objects?.map((t) => {
      if (typename !== t.name) {
        return t;
      }

      const relationships = t.relationships?.filter((r) => r.name !== relName);
      return {
        ...t,
        relationships,
      };
    }),
  };
};
