import React from 'react';
import { useQuery, useMutation } from '@tanstack/react-query';
import { GraphQLError } from 'graphql';
import {
  controlPlaneClient,
  GET_TENANT_ENV,
  UPDATE_TENANT_ENV,
} from '../../../../ControlPlane';
import { getTenantId } from '@hasura/shared/utils';
import { useNeonOAuth } from '../../../NeonOnboardingWizard/hooks/useNeonOAuth';
import { useNeonDatabase } from '../../../NeonOnboardingWizard/hooks/useNeonDatabase';
import { NeonIntegrationStatus } from '../../../NeonOnboardingWizard/hooks/useNeonIntegration';
import { verifyProjectHealthAndProceed } from './utils';
import {
  GetTenantEnvResponse,
  UpdateTenantEnvResponse,
  UpdateEnvObj,
} from './types';
import { getTenantEnvVarsQueryKey } from '../../constants';
import { useAppContext } from '@hasura/shared/context';

export const useGetTenantEnvs = () => {
  const { envVars } = useAppContext();

  return useQuery({
    queryKey: [getTenantEnvVarsQueryKey],
    queryFn: () => {
      const tenantId = getTenantId(envVars);
      if (!tenantId) {
        throw Error('Tenant was not found');
      }

      return controlPlaneClient.query<GetTenantEnvResponse>(GET_TENANT_ENV, {
        tenantId,
      });
    },
    // A stale time of 30 seconds before making a network call, stale time is required here
    // as main component re-renders on each click, making children components re-render.
    // This causes the hooks to run on each click. We only need this data on first render
    // so we can possibly set an even higher stale time.
    staleTime: 30 * 1000,
  });
};

export const useUpdateTenantEnv = (
  successCb: () => void,
  errorCb: (error?: GraphQLError) => void,
) => {
  const { envVars } = useAppContext();

  const updateTenantEnvMutationFn = (variables: Record<string, any>) => {
    const tenantId = getTenantId(envVars);
    if (!tenantId) {
      throw Error('Tenant was not found');
    }

    return controlPlaneClient.query<UpdateTenantEnvResponse>(
      UPDATE_TENANT_ENV,
      {
        tenantId,
        ...variables,
      },
    );
  };

  const mutation = useMutation({
    mutationFn: updateTenantEnvMutationFn,
    onSuccess: (data) => {
      // As graphql does not return error codes, react-query will always consider a
      // successful request, we have to parse the data to check for errors
      if (data.errors && data.errors.length > 0) {
        errorCb(data.errors[0]);
      } else {
        // Verify project health after 5 seconds since the project takes time to go down
        // and then further time to come back up.
        setTimeout(() => {
          verifyProjectHealthAndProceed(successCb, errorCb);
        }, 5000);
      }
    },
    // there might still be network errors, etc. which could be caught here
    onError: () => {
      errorCb();
    },
  });

  const onSubmitHandler = (hash: string, envVars: UpdateEnvObj[]) => {
    mutation.mutate({
      currentHash: hash,
      envs: envVars,
    });
  };

  return {
    onSubmitHandler,
  };
};

export const useNeonIntegrationForOneClickDeployment = () => {
  const { startNeonOAuth, neonOauthStatus } = useNeonOAuth();
  const {
    create: createNeonDatabase,
    state: neonDBCreationStatus,
    reset: resetNeonDBCreationState,
  } = useNeonDatabase();

  React.useEffect(() => {
    // automatically login if creating database fails with 401 unauthorized
    if (
      neonDBCreationStatus.status === 'error' &&
      neonDBCreationStatus.error === 'unauthorized' &&
      neonOauthStatus.status === 'idle'
    ) {
      startNeonOAuth();
      resetNeonDBCreationState();
    }

    // automatically create database after authentication completion
    if (
      neonOauthStatus.status === 'authenticated' &&
      (neonDBCreationStatus.status === 'idle' ||
        (neonDBCreationStatus.status === 'error' &&
          neonDBCreationStatus.error === 'unauthorized'))
    ) {
      createNeonDatabase();
    }
  }, [neonDBCreationStatus, neonOauthStatus]);

  const getNeonDBCreationStatus = (): NeonIntegrationStatus => {
    switch (neonDBCreationStatus.status) {
      case 'idle':
        return {
          status: 'idle',
          action: createNeonDatabase,
        };
      case 'error': {
        switch (neonOauthStatus.status) {
          case 'idle':
            if (neonDBCreationStatus.error === 'unauthorized') {
              return {
                status: 'idle',
                action: startNeonOAuth,
              };
            }
            return {
              status: 'neon-database-creation-error',
              action: createNeonDatabase,
              payload: {},
              title: 'Error creating Neon database',
              description: neonDBCreationStatus.error,
            };

          case 'error':
            return {
              status: 'authentication-error',
              payload: {},
              action: startNeonOAuth,
              title: 'Error authenticating with Neon',
              description: neonOauthStatus.error.message,
            };
          case 'authenticating':
            return {
              status: 'authentication-loading',
              payload: {},
            };
          case 'authenticated':
            return {
              status: 'neon-database-creation-error',
              action: createNeonDatabase,
              payload: {},
              title: 'Error creating Neon database',
              description: neonDBCreationStatus.error,
            };
          default:
            return {
              status: 'idle',
              action: startNeonOAuth,
            };
        }
      }
      case 'loading':
        return {
          status: 'neon-database-creation-loading',
          payload: {},
        };
      case 'success': {
        return {
          status: 'neon-database-creation-success',
          payload: {
            databaseUrl: neonDBCreationStatus.payload.databaseUrl || '',
            email: neonDBCreationStatus.payload.email || '',
          },
        };
      }
      default: {
        return {
          status: 'idle',
          payload: {},
          action: createNeonDatabase,
        };
      }
    }
  };

  const getNeonIntegrationStatus = (): NeonIntegrationStatus => {
    switch (neonOauthStatus.status) {
      case 'idle':
        return getNeonDBCreationStatus();
      case 'authenticating':
        return {
          status: 'authentication-loading',
          payload: {},
        };
      case 'error':
        return {
          status: 'authentication-error',
          payload: {},
          title: 'Error authenticating with Neon',
          description: neonOauthStatus.error.message,
          action: startNeonOAuth,
        };
      case 'authenticated':
        return getNeonDBCreationStatus();
      default:
        return {
          status: 'idle',
          payload: {},
          action: startNeonOAuth,
        };
    }
  };

  return getNeonIntegrationStatus();
};
