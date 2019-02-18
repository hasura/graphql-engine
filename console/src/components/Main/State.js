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
};

export default defaultState;
