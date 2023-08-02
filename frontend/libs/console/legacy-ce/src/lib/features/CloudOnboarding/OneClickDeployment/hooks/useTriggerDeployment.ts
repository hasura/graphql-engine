import {
  controlPlaneClient,
  TriggerOneClickDeploymentMutation,
  TriggerOneClickDeploymentMutationVariables,
  TRIGGER_ONE_CLICK_DEPLOYMENT,
} from '../../../ControlPlane';
import { useFireNotification } from '../../../../new-components/Notifications';
import { GraphQLError } from 'graphql';
import { useMutation } from 'react-query';

type TriggerOneClickDeploymentResponse = {
  data?: TriggerOneClickDeploymentMutation;
  errors?: GraphQLError[];
};

export const useTriggerDeployment = (projectId: string) => {
  const { fireNotification } = useFireNotification();

  const triggerOneClickDeploymentMutationFn = (variables: {
    projectId: string;
  }) => {
    return controlPlaneClient.query<
      TriggerOneClickDeploymentResponse,
      TriggerOneClickDeploymentMutationVariables
    >(TRIGGER_ONE_CLICK_DEPLOYMENT, variables);
  };

  const mutation = useMutation(triggerOneClickDeploymentMutationFn, {
    onSuccess: data => {
      // As graphql does not return error codes, react-query will always consider a
      // successful request, we have to parse the data to check for errors
      if (data.errors && data.errors.length > 0) {
        // http exception while calling webhook is already handled in the CLI screen UI, so we
        // don't want to display an additional error notification for it
        if (
          !data.errors[0].message ||
          data.errors[0].message !== 'http exception when calling webhook'
        ) {
          fireNotification({
            type: 'error',
            title: 'Error!',
            message: 'Something went wrong while triggering deployment',
            error: data.errors[0],
          });
        }
      }
    },
    // there might still be network errors, etc. which could be caught here
    onError: () => {
      fireNotification({
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
