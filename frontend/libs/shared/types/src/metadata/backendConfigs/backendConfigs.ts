export type BackendConfigs = {
  dataconnector: Record<string, DataConnectorBackendConfig>;
};

export type DataConnectorBackendConfig = {
  uri: DataConnectorUri;
};

export type DataConnectorUri = string | { from_env: string };
