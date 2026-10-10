import {
  controlPlaneClient,
  TriggerOneClickDeploymentMutation,
  TriggerOneClickDeploymentMutationVariables,
  TRIGGER_ONE_CLICK_DEPLOYMENT,
} from '../../../ControlPlane';
import { GraphQLError } from 'graphql';
import { useMutation } from '@tanstack/react-query';
import { hasuraToast } from '@hasura/shared/ui';
import { useErrorNotification } from '@hasura/metadata/api';

type TriggerOneClickDeploymentResponse = {
  data?: TriggerOneClickDeploymentMutation;
  errors?: GraphQLError[];
};

export const useTriggerDeployment = (projectId: string) => {
  const showErrorNotification = useErrorNotification();

  const triggerOneClickDeploymentMutationFn = (variables: {
    projectId: string;
  }) => {
    return controlPlaneClient.query<
      TriggerOneClickDeploymentResponse,
      TriggerOneClickDeploymentMutationVariables
    >(TRIGGER_ONE_CLICK_DEPLOYMENT, variables);
  };

  const mutation = useMutation({
    mutationFn: triggerOneClickDeploymentMutationFn,
    onSuccess: (data) => {
      // As graphql does not return error codes, react-query will always consider a
      // successful request, we have to parse the data to check for errors
      if (data.errors && data.errors.length > 0) {
        // http exception while calling webhook is already handled in the CLI screen UI, so we
        // don't want to display an additional error notification for it
        if (
          !data.errors[0].message ||
          data.errors[0].message !== 'http exception when calling webhook'
        ) {
          showErrorNotification({
            title: 'Triggering deployment failed',
            error: data.errors,
          });
        }
      }
    },
    // there might still be network errors, etc. which could be caught here
    onError: () => {
      hasuraToast({
        type: 'error',
        title: 'Error!',
        message: 'Something went wrong while triggering deployment',
      });
    },
  });

  const triggerDeployment = () => {
    mutation.mutate({
      projectId,
    });
  };

  return {
    triggerDeployment,
  };
};
