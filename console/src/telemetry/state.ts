import { Nullable } from '../components/Common/utils/tsUtils';

export type TelemetryState = {
  console_opts: Nullable<{
    telemetryNotificationShown?: boolean;
    disablePreReleaseUpdateNotifications?: boolean;
    consoleNotifications?: Record<string, any>;
  }>;
  hasura_uuid: string;
};

const defaultTelemetryState: TelemetryState = {
  console_opts: null,
  hasura_uuid: '',
};

export default defaultTelemetryState;
