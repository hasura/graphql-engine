import { Fragment, ReactNode, useCallback } from 'react';
import {
  FlattenCustomType,
  flattenCustomTypes,
  hydrateTypeRelationships,
  mergeFlatCustomTypes,
  reconstructCustomTypes,
} from '../../../shared/utils/hasuraCustomTypeUtils';
import { useMetadataMigration } from '@hasura/metadata/api';
import {
  hasuraToast,
  showErrorNotification,
  Text,
  useHasuraAlert,
} from '@hasura/shared/ui';
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
import { Code } from '@radix-ui/themes';

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
  const { mutateAsync, ...mutation } = useMetadataMigration();
  const { hasuraConfirm } = useHasuraAlert();

  const createAction = useCallback(
    // When overlapping types exist, returns a promise that settles when the
    // confirmation dialog closes (after the mutation is dispatched on confirm,
    // or immediately on cancel); otherwise it dispatches the mutation directly.
    // Either way callers can chain `.finally()` and keep their loading state
    // active while the confirmation is pending.
    async (
      {
        metadata,
        rawState,
        requestTransform,
        responseTransform,
      }: CreateActionArgs,
      // receives the name of the created action
      onSuccess?: (actionName: string) => unknown,
      onError?: (err: unknown) => void,
    ) => {
      const preparedData = prepareSaveActionData({ metadata, rawState });
      if (!preparedData) {
        return false;
      }

      const {
        state,
        allActions,
        existingTypesList,
        overlappingTypeNames,
        mergedTypes,
        actionComment,
      } = preparedData;

      const runMutation = () => {
        const customFieldsQueryUp = generateSetCustomTypesQuery(mergedTypes);

        const actionQueryUp = generateCreateActionQuery(
          state.name,
          generateActionDefinition(state, requestTransform, responseTransform),
          actionComment,
        );

        return mutateAsync({
          query: {
            type: 'bulk',
            args: [customFieldsQueryUp, actionQueryUp],
          },
        })
          .then(async () => {
            hasuraToast({
              title: 'Created action successfully!',
              type: 'success',
            });

            if (rawState.derive?.operation) {
              persistDerivedAction(state.name, rawState.derive.operation);
            }

            await onSuccess?.(state.name);

            return true;
          })
          .catch((error) => {
            showErrorNotification({
              title: 'Creating action failed!',
              error,
            });
            onError?.(error);

            return false;
          });
      };

      const overlappingTypeWarning = getOverlappingTypeWarning(
        state.name,
        allActions,
        existingTypesList,
        overlappingTypeNames,
      );

      if (overlappingTypeWarning) {
        return new Promise<boolean>((resolve) => {
          hasuraConfirm({
            title: 'Overlapping types',
            message: overlappingTypeWarning,
            confirmText: 'Continue',
            onClose: ({ confirmed }) => {
              if (!confirmed) {
                resolve(false);
                return;
              }

              runMutation().then(resolve);
            },
          });
        });
      }

      return runMutation();
    },
    [mutateAsync, hasuraConfirm],
  );

  return {
    ...mutation,
    createAction,
  };
};

// Returns a human-readable warning message when some of the action's custom
// types are also used by other actions, or `null` when there is no collision.
// Kept synchronous and side-effect free so the caller can decide how to surface
// the confirmation (previously this called `getConfirmation` directly).
const getOverlappingTypeWarning = (
  currentActionName: string,
  allActions: Action[],
  allTypes: FlattenCustomType[],
  overlappingTypeNames: string[],
): ReactNode => {
  const otherActions = allActions.filter((a) => a.name !== currentActionName);

  const typeCollisionMap: Record<string, boolean> = {};

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

  const collidingTypes = Object.keys(typeCollisionMap);
  const numCollidingTypes = collidingTypes.length;

  if (!numCollidingTypes) {
    return null;
  }

  const types = collidingTypes.map((t, i) => (
    <Fragment key={t}>
      <Code>{t}</Code>
      {i < collidingTypes.length - 1 ? ', ' : null}
    </Fragment>
  ));
  const typeLabel = numCollidingTypes === 1 ? 'type' : 'types';
  const verb = numCollidingTypes === 1 ? 'is' : 'are';

  return (
    <Text>
      The {typeLabel} {types} {verb} also used by other actions. Your current
      type definition will replace the existing type definition. This will
      impact existing actions
    </Text>
  );
};

export default useCreateAction;
