const defaultState = {
  migrationError: null,
  hasuractlEnv: null,
  migrationMode: true,
  migrationModeProgress: false,
  metadataExport: { error: false, info: null },
  adminSecretInput: null,
  loginInProgress: false,
  loginError: false,
  serverVersion: null,
  latestServerVersion: null,
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
};

export default defaultState;
