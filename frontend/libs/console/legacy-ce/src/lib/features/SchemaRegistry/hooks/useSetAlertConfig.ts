import * as React from 'react';
import { useMutation } from 'react-query';
import { SET_ALERT_CONFIG } from '../queries';
import { schemaRegsitryControlPlaneClient } from '../utils';
import { SetAlertConfigResponseWithError } from '../types';
import { FETCH_ALERT_CONFIG_QUERY_NAME } from '../constants';
import { reactQueryClient } from '../../../lib/reactQuery';
import { hasuraToast } from '../../../new-components/Toasts';

type setEmailAlertMutationFnArgs = {
  projectId: string;
  config: Record<string, boolean>;
};

export const useSetEmailAlertConfig = (onSuccess: VoidFunction) => {
  const setSchemaRegistryEmailAlertMutationFn = (variables: {
    projectId: string;
    config: Record<string, boolean>;
  }) => {
    return schemaRegsitryControlPlaneClient.query<
      SetAlertConfigResponseWithError,
      { projectId: string; config: Record<string, boolean> }
    >(SET_ALERT_CONFIG, variables);
  };

  const setEmailAlertMutation = useMutation(
    (args: setEmailAlertMutationFnArgs) =>
      setSchemaRegistryEmailAlertMutationFn(args),
    {
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          title: 'Success!',
          message: 'Email configuration set successfully!',
        });

        onSuccess();
      },
      onError: () => {
        hasuraToast({
          type: 'error',
          title: 'Error!',
          message:
            'Something went wrong while setting alerts for Schema Registry',
        });
      },
      onSettled: response => {
        if (response?.data) {
          reactQueryClient.refetchQueries(FETCH_ALERT_CONFIG_QUERY_NAME);
        }
      },
    }
  );

  return {
    setEmailAlertMutation,
  };
};
