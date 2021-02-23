import { ConsoleNotification } from './ConsoleNotification';
import { HerokuSession } from '../Services/Data/DataSources/CreateDataSource/Heroku/types';

export interface MainState {
  migrationError: unknown | null;
  hasuractlEnv: unknown | null;
  migrationMode: boolean;
  readOnlyMode: boolean;
  migrationModeProgress: boolean;
  metadataExport: { error: boolean; info: unknown | null };
  adminSecretInput: unknown | null;
  loginInProgress: boolean;
  loginError: boolean;
  serverVersion: null;
  latestStableServerVersion: null | string;
  latestPreReleaseServerVersion: null | string;
  telemetryEnabled: boolean;
  serverConfig: {
    data: {
      version: string;
      is_function_permissions_inferred: boolean;
      is_admin_secret_set: boolean;
      is_auth_hook_set: boolean;
      is_jwt_set: boolean;
      jwt: {
        claims_namespace: string;
        claims_format: string;
      };
    };
    error: Error | null;
    isFetching: boolean;
  };
  featuresCompatibility: Record<string, unknown>;
  postgresVersion: string | null;
  consoleNotifications: ConsoleNotification[];
  heroku: {
    session?: HerokuSession;
  };
}

const defaultState: MainState = {
  migrationError: null,
  hasuractlEnv: null,
  migrationMode: true,
  readOnlyMode: false,
  migrationModeProgress: false,
  metadataExport: { error: false, info: null },
  adminSecretInput: null,
  loginInProgress: false,
  loginError: false,
  serverVersion: null,
  latestStableServerVersion: null,
  latestPreReleaseServerVersion: null,
  telemetryEnabled: true,
  serverConfig: {
    data: {
      version: '',
      is_function_permissions_inferred: true,
      is_admin_secret_set: false,
      is_auth_hook_set: false,
      is_jwt_set: false,
      jwt: {
        claims_namespace: '',
        claims_format: '',
      },
    },
    error: null,
    isFetching: false,
  },
  featuresCompatibility: {},
  postgresVersion: null,
  consoleNotifications: [],
  heroku: {
    session: undefined,
  },
};

export default defaultState;
