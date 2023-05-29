import React from 'react';
import { REGISTER_EE_TRIALS_MUTATION } from '../../../constants';
import globals from '../../../../../Globals';
import { EETrialRegistrationResponse } from '../../../types';
import { GraphQLError } from 'graphql';
import { useMutation } from 'react-query';
import { RegisterEEFormSchema } from './schema';
import { eeTrialsControlPlaneClient } from '../../../utils';
import { useClientCredentialsPost } from './useClientCredentialsPost';

export type RegisterEeTrialResponseWithError = {
  data?: EETrialRegistrationResponse;
  errors?: GraphQLError[];
};

export type RegisterEeTrialMutationVariables = {
  first: string;
  last: string;
  email: string;
  jobFunction: string;
  organization: string;
  phone: string;
  password: string;
  hasuraUseCase: string;
  eeUseCase: string[];
};

const registerEETrialMutationFn = (formData: RegisterEEFormSchema) => {
  return eeTrialsControlPlaneClient.query<
    RegisterEeTrialResponseWithError,
    RegisterEeTrialMutationVariables
  >(REGISTER_EE_TRIALS_MUTATION, {
    first: formData.firstName,
    last: formData.lastName,
    email: formData.email,
    jobFunction: formData.jobFunction,
    organization: formData.organization,
    phone: formData.phoneNumber as string,
    // password: formData.password
    password: formData.password,
    hasuraUseCase: formData.hasuraUseCase,
    eeUseCase: formData.eeUseCase,
  });
};

export const useRegisterEETrial = (onSuccess?: VoidFunction) => {
  const [errorMessage, setErrorMessage] = React.useState('');

  const { post: postClientCredentials } = useClientCredentialsPost(
    onSuccess,
    msg => setErrorMessage('Error: ' + msg)
  );

  const { mutate, isLoading } = useMutation(registerEETrialMutationFn, {
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

  const registerEETrial = (formData: RegisterEEFormSchema) => {
    mutate(formData);
  };

  return {
    registerEETrial,
    isLoading,
    errorMessage,
  };
};
