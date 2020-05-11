export interface TelemetryState {
  console_opts: object | null;
  hasura_uuid: string | null;
}

const defaultTelemetryState: TelemetryState = {
  console_opts: null,
  hasura_uuid: null,
};

export default defaultTelemetryState;
