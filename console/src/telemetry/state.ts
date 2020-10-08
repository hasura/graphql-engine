import { Nullable } from '../components/Common/utils/tsUtils';

export type ConsoleState = {
  console_opts: Nullable<{
    telemetryNotificationShown?: boolean;
    disablePreReleaseUpdateNotifications?: boolean;
  }>;
  hasura_uuid: string;
};

const defaultConsoleState: ConsoleState = {
  console_opts: null,
  hasura_uuid: '',
};

export default defaultConsoleState;
