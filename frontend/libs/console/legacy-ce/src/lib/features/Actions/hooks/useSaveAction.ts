import { useCallback } from 'react';
import {
  TMigrationSingleQuery,
  useMetadataMigration,
} from '@hasura/metadata/api';
import { getErrorMessage, getConfirmation } from '@hasura/shared/utils';
import { hasuraToast } from '@hasura/shared/ui';
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
  const mutation = useMetadataMigration();
  const navigate = useNavigate();

  return useCallback(
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
        return;
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

      const upArgs: TMigrationSingleQuery[] = [];
      // Migration queries start
      if (!isActionNameChange) {
        upArgs.push(customFieldsQueryUp, updateCurrentActionQuery);
      } else {
        const isOk = getConfirmation(
          'You seem to have changed the action name. This will cause the permissions to be dropped.',
        );

        if (!isOk) return;
        upArgs.push(
          dropCurrentActionQuery,
          customFieldsQueryUp,
          createNewActionQuery,
        );
      }

      return mutation.mutate(
        {
          query: {
            type: 'bulk',
            args: upArgs,
          },
        },
        {
          onSuccess: () => {
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
          },
          onError: (err) => {
            hasuraToast({
              title: 'Saving action failed!',
              message: getErrorMessage(err),
              type: 'error',
            });
          },
        },
      );
    },
    [mutation],
  );
};

export default useSaveAction;
