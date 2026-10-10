import * as React from 'react';
import { useMutation, useQueryClient } from '@tanstack/react-query';
import { SET_ALERT_CONFIG } from '../queries';
import { SetAlertConfigResponseWithError } from '../types';
import { FETCH_ALERT_CONFIG_QUERY_NAME } from '../constants';
import { hasuraToast } from '@hasura/shared/ui';
import { controlPlaneClient } from '../../ControlPlane';

type setEmailAlertMutationFnArgs = {
  projectId: string;
  rules: Record<string, boolean>;
};

export const useSetEmailAlertConfig = (onSuccess: VoidFunction) => {
  const queryClient = useQueryClient();
  const setSchemaRegistryEmailAlertMutationFn = (variables: {
    projectId: string;
    rules: Record<string, boolean>;
  }) => {
    return controlPlaneClient.query<
      SetAlertConfigResponseWithError,
      { projectId: string; rules: Record<string, boolean> }
    >(SET_ALERT_CONFIG, variables);
  };

  const setEmailAlertMutation = useMutation({
    mutationFn: (args: setEmailAlertMutationFnArgs) =>
      setSchemaRegistryEmailAlertMutationFn(args),
    onSuccess: (data) => {
      if (data.errors && data.errors.length > 0) {
        hasuraToast({
          type: 'error',
          title: 'Error!',
          message: 'Something unexpected happened!',
        });
      } else {
        hasuraToast({
          type: 'success',
          title: 'Success!',
          message: 'Email configuration set successfully!',
        });

        if (data.data) onSuccess();
      }
    },
    onError: () => {
      hasuraToast({
        type: 'error',
        title: 'Error!',
        message:
          'Something went wrong while setting alerts for Schema Registry',
      });
    },
    onSettled: (response) => {
      if (response?.data) {
        queryClient.refetchQueries({
          queryKey: FETCH_ALERT_CONFIG_QUERY_NAME,
        });
      }
    },
  });

  return {
    setEmailAlertMutation,
  };
};
