import React from 'react';
import globals from '../../../../../Globals';
import { ACTIVATE_EE_TRIALS_MUTATION } from '../../../constants';
import { EETrialRegistrationResponse } from '../../../types';
import { GraphQLError } from 'graphql';
import { useMutation } from 'react-query';
import { ActivateEEFormSchema } from './schema';
import { eeTrialsControlPlaneClient } from '../../../utils';
import { useClientCredentialsPost } from './useClientCredentialsPost';

export type ActivateEeTrialResponseWithError = {
  data?: EETrialRegistrationResponse;
  errors?: GraphQLError[];
};

export type ActivateEeTrialMutationVariables = {
  email: string;
  password: string;
};

const activateEETrialMutationFn = (formData: ActivateEEFormSchema) => {
  return eeTrialsControlPlaneClient.query<
    ActivateEeTrialResponseWithError,
    ActivateEeTrialMutationVariables
  >(ACTIVATE_EE_TRIALS_MUTATION, {
    email: formData.email,
    password: formData.password,
  });
};

export const useActivateEETrial = (onSuccess?: VoidFunction) => {
  const [errorMessage, setErrorMessage] = React.useState('');

  const { post: postClientCredentials } = useClientCredentialsPost(
    onSuccess,
    msg => setErrorMessage('Error: ' + msg)
  );

  const { mutate, isLoading } = useMutation(activateEETrialMutationFn, {
    onSuccess: data => {
      if (data.data?.registerEETrial?.client_id) {
        setErrorMessage('');
        postClientCredentials({
          clientId: data.data?.registerEETrial?.client_id,
          clientSecret: data.data?.registerEETrial?.client_secret,
          adminSecret: globals.adminSecret || '',
        });
      } else if (data.errors && data.errors.length > 0) {
        // As graphql does not return error codes, react-query will always consider a
        // successful request, we have to parse the data to check for errors
        setErrorMessage('Error: ' + data.errors[0].message);
      } else {
        setErrorMessage(
          'Something went wrong while activating your Enterprise Trial'
        );
      }
    },
    // there might still be network errors, etc. which could be caught here
    onError: (error: Error) => {
      setErrorMessage('Error: ' + error.message);
    },
  });

  const activateEETrial = (formData: ActivateEEFormSchema) => {
    mutate(formData);
  };

  return {
    activateEETrial,
    isLoading,
    errorMessage,
  };
};
