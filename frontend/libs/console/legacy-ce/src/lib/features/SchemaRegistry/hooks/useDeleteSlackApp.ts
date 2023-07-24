import * as React from 'react';
import { useMutation, useQueryClient } from 'react-query';
import { DELETE_SLACK_APP } from '../queries';
import { DeleteSlackAppMutationResponseWithError } from '../types';
import { FETCH_SLACK_STATE_QUERY_NAME } from '../constants';
import { hasuraToast } from '../../../new-components/Toasts';
import { controlPlaneClient } from '../../ControlPlane';

type DeleteSlackAppMutationFnArgs = {
  projectID: string;
};

export const useDeleteSlackApp = (onClose: () => void) => {
  const deleteSlackAppMutationFn = (variables: { projectID: string }) => {
    return controlPlaneClient.query<
      DeleteSlackAppMutationResponseWithError,
      { projectID: string }
    >(DELETE_SLACK_APP, variables);
  };

  const queryClient = useQueryClient();

  const deleteSlackApp = useMutation(
    (args: DeleteSlackAppMutationFnArgs) => deleteSlackAppMutationFn(args),
    {
      onSuccess: response => {
        if (response.errors && response.errors.length > 0) {
          hasuraToast({
            type: 'error',
            title: 'Error!',
            message:
              'Something unexpected happened while deleting Slack Alerts!',
          });
        } else {
          hasuraToast({
            type: 'success',
            title: 'Success!',
            message: 'Slack Integration deleted successfully',
          });

          if (response?.data) {
            queryClient.invalidateQueries(FETCH_SLACK_STATE_QUERY_NAME);
            onClose();
          }
        }
      },
      onError: () => {
        hasuraToast({
          type: 'error',
          title: 'Error!',
          message:
            'Something went wrong while deleting the tag for Schema Registry',
        });
      },
    }
  );

  return {
    deleteSlackApp,
  };
};
