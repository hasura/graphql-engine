import React, { useEffect } from 'react';
import { Dispatch } from '../../../../../../types';
import { useNeonOAuth } from './useNeonOAuth';
import { useNeonDatabase } from './useNeonDatabase';
import { useCreateHasuraCloudDatasource } from './useCreateHasuraCloudDatasource';
import { setDBConnectionDetails } from '../../../DataActions';
import { NeonIntegrationContext } from './utils';

type EmptyPayload = Record<string, never>;

type NeonDBCreationSuccessPayload = {
  databaseUrl: string;
  email: string;
};

type EnvVarCreationPayload = {
  databaseUrl: string;
  dataSourceName: string;
};
type DatasourceCreationPayload = {
  envVar: string;
  databaseUrl: string;
  dataSourceName: string;
};

type Idle<Status, Payload> = {
  status: Status;
  payload?: Payload;
  action: VoidFunction;
};

type Loading<Status, Payload> = {
  status: Status;
  payload: Payload;
};

type Error<Status, Payload> = {
  status: Status;
  payload: Payload;
  title: string;
  description: string | React.ReactNode;
  action: VoidFunction;
};

type Success<Status, Payload> = {
  status: Status;
  payload: Payload;
};

export type NeonIntegrationStatus =
  | Idle<'idle', EmptyPayload>
  | Loading<'authentication-loading', EmptyPayload>
  | Success<'authentication-success', EmptyPayload>
  | Error<'authentication-error', EmptyPayload>
  | Loading<'neon-database-creation-loading', EmptyPayload>
  | Success<'neon-database-creation-success', NeonDBCreationSuccessPayload>
  | Error<'neon-database-creation-error', EmptyPayload>
  | Loading<'env-var-creation-loading', EnvVarCreationPayload>
  | Success<'env-var-creation-success', DatasourceCreationPayload>
  | Error<'env-var-creation-error', EnvVarCreationPayload>
  | Loading<'hasura-source-creation-loading', DatasourceCreationPayload>
  | Success<'hasura-source-creation-success', DatasourceCreationPayload>
  | Error<'hasura-source-creation-error', DatasourceCreationPayload>;

export function useNeonIntegration(
  dataSourceName: string,
  dbCreationCallback: (dataSourceName: string) => void, // TODO use NeonIntegrationStatus as a parameter
  failureCallback: VoidFunction, // TODO use NeonIntegrationStatus as a parameter
  dispatch: Dispatch,
  context: NeonIntegrationContext
): NeonIntegrationStatus {
  const { startNeonOAuth, neonOauthStatus } = useNeonOAuth();

  const {
    create: createNeonDatabase,
    state: neonDBCreationStatus,
    reset: resetNeonDBCreationState,
  } = useNeonDatabase();

  const { state: hasuraCloudDataSourceConnectionStatus, addHasuraDatasource } =
    useCreateHasuraCloudDatasource(
      neonDBCreationStatus.status === 'success'
        ? neonDBCreationStatus.payload.databaseUrl || ''
        : '',
      dataSourceName,
      dispatch,
      context
    );

  useEffect(() => {
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

  useEffect(() => {
    if (neonDBCreationStatus.status === 'success') {
      switch (hasuraCloudDataSourceConnectionStatus.status) {
        case 'idle':
          addHasuraDatasource();
          break;
        case 'adding-env-var-failed':
          dispatch(
            setDBConnectionDetails({
              dbURL: hasuraCloudDataSourceConnectionStatus.payload.dbUrl,
              dbName: 'default',
            })
          );
          failureCallback();
          break;
        case 'adding-data-source-failed':
          dispatch(
            setDBConnectionDetails({
              envVar: hasuraCloudDataSourceConnectionStatus.payload.envVar,
              dbName: 'default',
            })
          );
          failureCallback();
          break;
        case 'success':
          dbCreationCallback(
            hasuraCloudDataSourceConnectionStatus.payload.dataSourceName
          );
          break;
        default:
          break;
      }
    }
  }, [neonDBCreationStatus, hasuraCloudDataSourceConnectionStatus]);

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
        break;
      }
      case 'loading':
        return {
          status: 'neon-database-creation-loading',
          payload: {},
        };
      case 'success':
        {
          const { databaseUrl: dbUrl } = neonDBCreationStatus.payload;
          switch (hasuraCloudDataSourceConnectionStatus.status) {
            case 'idle':
              return {
                status: 'neon-database-creation-success',
                payload: {
                  databaseUrl: neonDBCreationStatus.payload.databaseUrl || '',
                  email: neonDBCreationStatus.payload.email || '',
                },
              };
            case 'adding-env-var':
              return {
                status: 'env-var-creation-loading',
                payload: {
                  databaseUrl: neonDBCreationStatus.payload.databaseUrl || '',
                  dataSourceName,
                },
              };
            case 'adding-env-var-failed':
              return {
                status: 'env-var-creation-error',
                payload: {
                  databaseUrl: dbUrl || '',
                  dataSourceName,
                },
                action: () => null,
                title: 'Error creating env var',
                description:
                  'Unexpected error adding env vars to the Hasura Cloud project',
              };
            case 'adding-data-source':
              return {
                status: 'hasura-source-creation-loading',
                payload: {
                  dataSourceName,
                  envVar: hasuraCloudDataSourceConnectionStatus.payload.envVar,
                  databaseUrl: dbUrl || '',
                },
              };
            case 'success':
              return {
                status: 'hasura-source-creation-success',
                payload: {
                  databaseUrl: dbUrl || '',
                  dataSourceName,
                  envVar: hasuraCloudDataSourceConnectionStatus.payload.envVar,
                },
              };
            case 'adding-data-source-failed':
            default:
              return {
                status: 'hasura-source-creation-error',
                payload: {
                  databaseUrl: dbUrl || '',
                  dataSourceName,
                  envVar: hasuraCloudDataSourceConnectionStatus.payload.envVar,
                },
                action: createNeonDatabase,
                title: 'Error connecting database to Hasura',
                description:
                  'Unexpected error connecting the database to Hasura',
              };
          }
        }
        break;
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
}
