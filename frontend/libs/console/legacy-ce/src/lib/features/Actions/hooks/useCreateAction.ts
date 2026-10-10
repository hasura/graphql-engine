import { useCallback } from 'react';
import {
  FlattenCustomType,
  flattenCustomTypes,
  hydrateTypeRelationships,
  mergeFlatCustomTypes,
  reconstructCustomTypes,
} from '../../../shared/utils/hasuraCustomTypeUtils';
import { useMetadataMigration } from '@hasura/metadata/api';
import { getErrorMessage, getConfirmation } from '@hasura/shared/utils';
import { hasuraToast } from '@hasura/shared/ui';
import {
  Action,
  ActionDefinition,
  ActionRequestTransform,
  CustomTypes,
  Metadata,
  ResponseTransform,
} from '@hasura/shared/types';
import {
  buildCustomTypesFromSDL,
  getActionDefinitionFromSdl,
} from '../../../shared/utils/sdlUtils';
import {
  generateActionDefinition,
  getActionTypes,
  getStateValidationError,
} from '../components/Form/utils';
import { generateSetCustomTypesQuery } from './useSetCustomGraphQLTypes';
import { persistDerivedAction } from '../utils';
import { ActionState } from '../types';

type CreateActionArgs = {
  metadata: Metadata['metadata'];
  rawState: ActionState;
  requestTransform: ActionRequestTransform | null;
  responseTransform: ResponseTransform | null;
};

export const generateCreateActionQuery = (
  name: string,
  definition: ActionDefinition,
  comment: string | null,
) => {
  return {
    type: 'create_action' as const,
    args: {
      name,
      definition,
      comment,
    },
  };
};

export const prepareSaveActionData = ({
  metadata,
  rawState,
}: {
  metadata: Metadata['metadata'];
  rawState: ActionState;
}) => {
  const actionComment = rawState.comment ? rawState.comment.trim() : null;
  const {
    name: actionName,
    arguments: args,
    outputType,
    error: actionDefError,
    type: actionType,
  } = getActionDefinitionFromSdl(rawState.actionDefinition.sdl);
  if (actionDefError) {
    hasuraToast({
      type: 'error',
      title: 'Invalid Action Definition',
      message: actionDefError,
    });

    return null;
  }

  const { types, error: typeDefError } = buildCustomTypesFromSDL(
    rawState.typeDefinition.sdl,
  );

  if (typeDefError) {
    hasuraToast({
      type: 'error',
      title: 'Invalid Types Definition',
      message: typeDefError,
    });

    return null;
  }

  const state = {
    handler: rawState.handler.trim(),
    kind: rawState.kind,
    types,
    actionType,
    name: actionName,
    arguments: args,
    outputType,
    headers: rawState.headers,
    comment: actionComment,
    timeout: parseInt(rawState.timeout, 10),
    forwardClientHeaders: rawState.forwardClientHeaders,
  };

  const validationError = getStateValidationError(state);
  if (validationError) {
    hasuraToast({
      type: 'error',
      title: 'Validation Error',
      message: validationError,
    });

    return null;
  }

  const existingTypes = metadata.custom_types;
  const allActions = metadata.actions ?? [];

  const typesWithRelationships = existingTypes
    ? hydrateTypeRelationships(state.types, existingTypes)
    : state.types;

  const existingTypesList = existingTypes
    ? flattenCustomTypes(existingTypes)
    : [];
  const newTypesList = flattenCustomTypes(typesWithRelationships);
  let overlappingTypeNames: string[] = [];
  let mergedTypes: CustomTypes = typesWithRelationships;

  if (existingTypes) {
    const result = mergeFlatCustomTypes(newTypesList, existingTypesList);
    overlappingTypeNames = result.overlappingTypeNames;
    mergedTypes = reconstructCustomTypes(result.types);
  }

  return {
    state,
    allActions,
    existingTypesList,
    overlappingTypeNames,
    mergedTypes,
    actionComment,
  };
};

const useCreateAction = () => {
  const mutation = useMetadataMigration();

  return useCallback(
    async (
      {
        metadata,
        rawState,
        requestTransform,
        responseTransform,
      }: CreateActionArgs,
      onSuccess?: () => unknown,
      onError?: (err: unknown) => void,
    ) => {
      const preparedData = prepareSaveActionData({ metadata, rawState });
      if (!preparedData) {
        return;
      }
      const {
        state,
        allActions,
        existingTypesList,
        overlappingTypeNames,
        mergedTypes,
        actionComment,
      } = preparedData;

      if (overlappingTypeNames) {
        const isOk = getOverlappingTypeConfirmation(
          state.name,
          allActions,
          existingTypesList,
          overlappingTypeNames,
        );
        if (!isOk) {
          return;
        }
      }

      const customFieldsQueryUp = generateSetCustomTypesQuery(mergedTypes);

      const actionQueryUp = generateCreateActionQuery(
        state.name,
        generateActionDefinition(state, requestTransform, responseTransform),
        actionComment,
      );

      return mutation.mutate(
        {
          query: {
            type: 'bulk',
            args: [customFieldsQueryUp, actionQueryUp],
          },
        },
        {
          onSuccess: () => {
            onSuccess?.();

            hasuraToast({
              title: 'Created action successfully!',
              type: 'success',
            });

            if (rawState.derive?.operation) {
              persistDerivedAction(state.name, rawState.derive.operation);
            }
          },
          onError: (err) => {
            hasuraToast({
              title: 'Creating action failed!',
              message: getErrorMessage(err),
              type: 'error',
            });
            onError?.(err);
          },
        },
      );
    },
    [mutation],
  );
};

const getOverlappingTypeConfirmation = (
  currentActionName: string,
  allActions: Action[],
  allTypes: FlattenCustomType[],
  overlappingTypeNames: string[],
) => {
  const otherActions = allActions.filter((a) => a.name !== currentActionName);

  const typeCollisionMap = {};

  for (let i = otherActions.length - 1; i >= 0; i--) {
    const action = otherActions[i];
    const actionTypes = getActionTypes(action, allTypes);
    actionTypes.forEach((t) => {
      if (!t || typeCollisionMap[t.definition.name]) return;
      overlappingTypeNames.forEach((ot) => {
        if (ot === t.definition.name) {
          typeCollisionMap[ot] = true;
        }
      });
    });
  }

  let isOk = true;
  const collidingTypes = Object.keys(typeCollisionMap);
  const numCollidingTypes = collidingTypes.length;

  if (numCollidingTypes) {
    const types = `${collidingTypes.join(', ')}`;
    const typeLabel = numCollidingTypes === 1 ? 'type' : 'types';
    const verb = numCollidingTypes === 1 ? 'is' : 'are';
    isOk = getConfirmation(
      `The ${typeLabel} "${types}" ${verb} also used by other actions. Your current type definition will replace the existing type definition. This will impact existing actions`,
    );
  }

  return isOk;
};

export default useCreateAction;
