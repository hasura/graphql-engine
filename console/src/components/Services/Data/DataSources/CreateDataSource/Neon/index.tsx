import * as React from 'react';
import { useDispatch } from 'react-redux';
import { useNeonOAuth } from './useNeonOAuth';
import { useNeonDatabase } from './useNeonDatabase';
import { useCreateHasuraDatasource } from './useCreateHasuraDatasource';
import { setDBConnectionDetails } from '../../../DataActions';
import {
  NeonBanner,
  Props as NeonBannerProps,
} from './components/Neon/NeonBanner';
import { getNeonDBName } from './utils';

// This component creates Neon DB and calls the success/error callback
export function Neon(props: {
  oauthString?: string;
  dbCreationCallback: (dbName: string) => void;
  errorCallback: VoidFunction;
  allDatabases: string[];
}) {
  const dispatch = useDispatch();
  const {
    state: neonDBCreationStatus,
    create: createNeonDatabase,
    reset: resetNeonDBCreationState,
  } = useNeonDatabase();

  const { startNeonOAuth, neonOauthStatus } = useNeonOAuth(props.oauthString);

  const { state: hasuraDatasourceCreationState, addHasuraDatasource } =
    useCreateHasuraDatasource(
      neonDBCreationStatus.status === 'success'
        ? neonDBCreationStatus.payload.databaseUrl || ''
        : '',
      getNeonDBName(props.allDatabases)
    );

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

  React.useEffect(() => {
    if (neonDBCreationStatus.status === 'success') {
      switch (hasuraDatasourceCreationState.status) {
        case 'idle':
          addHasuraDatasource();
          break;
        case 'adding-env-var-failed':
          dispatch(
            setDBConnectionDetails({
              dbURL: hasuraDatasourceCreationState.payload.dbUrl,
              dbName: 'default',
            })
          );
          props.errorCallback();
          break;
        case 'adding-data-source-failed':
          dispatch(
            setDBConnectionDetails({
              envVar: hasuraDatasourceCreationState.payload.envVar,
              dbName: 'default',
            })
          );
          props.errorCallback();
          break;
        case 'success':
          props.dbCreationCallback(
            hasuraDatasourceCreationState.payload.dataSourceName
          );
          break;
        default:
          break;
      }
    }
  }, [neonDBCreationStatus, hasuraDatasourceCreationState]);

  // gets the NeonBanner render props associated with Neon DB creation
  let neonDBCreationStatusOpts: NeonBannerProps;
  switch (neonDBCreationStatus.status) {
    case 'idle':
      neonDBCreationStatusOpts = {
        status: { status: 'default', buttonText: 'Connect Neon Database' },
        onClickConnect: createNeonDatabase,
      };
      break;
    case 'error': {
      switch (neonDBCreationStatus.error) {
        case 'unauthorized':
          neonDBCreationStatusOpts = {
            status: {
              status: 'default',
              buttonText: 'Connect Neon Database',
            },
            onClickConnect: startNeonOAuth,
          };
          break;
        default:
          neonDBCreationStatusOpts = {
            status: {
              status: 'error',
              buttonText: 'Try again',
              errorTitle: 'Creating Neon Database failed',
              errorDescription: `Error creating database: ${neonDBCreationStatus.error}`,
            },
            onClickConnect: createNeonDatabase,
          };
          break;
      }
      break;
    }
    case 'loading':
      neonDBCreationStatusOpts = {
        status: { status: 'loading', buttonText: 'Creating Database' },
        onClickConnect: () => null,
      };
      break;
    case 'success':
      neonDBCreationStatusOpts = {
        status: { status: 'loading', buttonText: 'Connecting to Hasura' },
        onClickConnect: () => null,
      };
      break;
    default:
      // never happens; handling for placating TypeScript
      neonDBCreationStatusOpts = {
        status: {
          status: 'default',
          buttonText: 'Create Neon Database',
        },
        onClickConnect: createNeonDatabase,
      };
      break;
  }

  // get the NeonBanner render props associated with Neon OAuth
  let neonOAuthStatusOpts: NeonBannerProps;
  switch (neonOauthStatus.status) {
    case 'idle':
      neonOAuthStatusOpts = { ...neonDBCreationStatusOpts };
      break;
    case 'authenticating':
      neonOAuthStatusOpts = {
        status: { status: 'loading', buttonText: 'Authenticating with Neon' },
        onClickConnect: () => null,
      };
      break;
    case 'authenticated':
      neonOAuthStatusOpts = { ...neonDBCreationStatusOpts };
      break;
    case 'error':
      neonOAuthStatusOpts = {
        status: {
          status: 'error',
          buttonText: 'Try again',
          errorTitle: 'Error authenticating with Neon',
          errorDescription: neonOauthStatus.error.message,
        },
        onClickConnect: startNeonOAuth,
      };
      break;
    default:
      // never happens; handling for placating TypeScript
      neonOAuthStatusOpts = {
        status: {
          status: 'default',
          buttonText: 'Create Neon Database',
        },
        onClickConnect: createNeonDatabase,
      };
      break;
  }

  return <NeonBanner {...neonOAuthStatusOpts} />;
}
