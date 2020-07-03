import {
  ConsoleNotification,
  defaultNotification,
} from './ConsoleNotification';

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
  latestStableServerVersion: null;
  telemetryEnabled: boolean;
  serverConfig: {
    data: {
      version: string;
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
  featuresCompatibility: object;
  consoleNotifications: ConsoleNotification[];
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
  telemetryEnabled: true,
  serverConfig: {
    data: {
      version: '',
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
  consoleNotifications: [defaultNotification],
};

export default defaultState;
