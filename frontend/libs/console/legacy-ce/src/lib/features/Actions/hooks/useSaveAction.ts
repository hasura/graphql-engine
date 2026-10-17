import { useCallback } from 'react';
import {
  TMigrationSingleQuery,
  useMetadataMigration,
} from '@hasura/metadata/api';
import {
  hasuraToast,
  showErrorNotification,
  useHasuraAlert,
} from '@hasura/shared/ui';
import {
  Action,
  ActionDefinition,
  ActionRequestTransform,
  Metadata,
  ResponseTransform,
} from '@hasura/shared/types';
import {
  generateCreateActionQuery,
  prepareSaveActionData,
} from './useCreateAction';
import { generateDropActionQuery } from './useDeleteAction';
import { useNavigate } from 'react-router';
import { ActionState } from '../types';
import { generateSetCustomTypesQuery } from './useSetCustomGraphQLTypes';
import { updatePersistedDerivation } from '../utils';
import { generateActionDefinition } from '../components/Form/utils';

type SaveActionArgs = {
  metadata: Metadata['metadata'];
  rawState: ActionState;
  currentAction: Action;
  requestTransform: ActionRequestTransform | null;
  responseTransform: ResponseTransform | null;
};

const getUpdateActionQuery = (
  def: ActionDefinition,
  actionName: string,
  actionComment?: string | null,
  request_transform?: ActionRequestTransform | null,
) => {
  return {
    type: 'update_action' as const,
    args: {
      name: actionName,
      definition: def,
      comment: actionComment,
      request_transform,
    },
  };
};

const useSaveAction = () => {
  const navigate = useNavigate();
  const { mutateAsync, ...mutation } = useMetadataMigration();
  const { hasuraConfirm } = useHasuraAlert();

  const saveAction = useCallback(
    async (
      {
        metadata,
        rawState,
        currentAction,
        requestTransform,
        responseTransform,
      }: SaveActionArgs,
      onSuccess?: () => unknown,
    ) => {
      const preparedData = prepareSaveActionData({ metadata, rawState });
      if (!preparedData) {
        return false;
      }

      const { state, mergedTypes, actionComment } = preparedData;

      const isActionNameChange = currentAction.name !== state.name;

      const customFieldsQueryUp = generateSetCustomTypesQuery(mergedTypes);
      const dropCurrentActionQuery = generateDropActionQuery(
        currentAction.name,
      );
      const updateCurrentActionQuery = getUpdateActionQuery(
        generateActionDefinition(state, requestTransform, responseTransform),
        currentAction.name,
        actionComment,
      );

      const createNewActionQuery = generateCreateActionQuery(
        state.name,
        generateActionDefinition(state, requestTransform, responseTransform),
        actionComment,
      );

      const runMutation = (upArgs: TMigrationSingleQuery[]) => {
        return mutateAsync({
          query: {
            type: 'bulk',
            args: upArgs,
          },
        })
          .then(() => {
            onSuccess?.();

            hasuraToast({
              title: 'Action saved successfully!',
              type: 'success',
            });

            if (isActionNameChange) {
              updatePersistedDerivation(currentAction.name, state.name);
              const newHref = `/manage/${state.name}/modify`;
              navigate(newHref);
            }

            return true;
          })
          .catch((error) => {
            showErrorNotification({
              title: 'Saving action failed!',
              error,
            });

            return false;
          });
      };

      // Migration queries start
      if (!isActionNameChange) {
        return runMutation([customFieldsQueryUp, updateCurrentActionQuery]);
      }

      // Renaming an action drops and recreates it, which drops its permissions.
      // Resolves `false` on cancel, otherwise with the mutation's outcome once
      // it completes, so callers tracking loading stay active until then.
      return new Promise<boolean>((resolve) => {
        hasuraConfirm({
          title: 'Rename action',
          message:
            'You seem to have changed the action name. This will cause the permissions to be dropped.',
          destructive: true,
          confirmText: 'Save',
          onClose: ({ confirmed }) => {
            if (!confirmed) {
              resolve(false);
              return;
            }

            runMutation([
              dropCurrentActionQuery,
              customFieldsQueryUp,
              createNewActionQuery,
            ]).then(resolve);
          },
        });
      });
    },
    [mutateAsync, navigate, hasuraConfirm],
  );

  return {
    saveAction,
    ...mutation,
  };
};

export default useSaveAction;
