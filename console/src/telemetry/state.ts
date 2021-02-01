import { Nullable } from '../components/Common/utils/tsUtils';

export type NotificationsState = {
  read: 'all' | 'default' | 'error' | string[];
  date: string | null; // ISO String
  showBadge: boolean;
};

export type UserTypes = 'admin' | string;
export type TelemetryNotificationsState = Record<UserTypes, NotificationsState>;

export type ConsoleState = {
  console_opts: Nullable<{
    telemetryNotificationShown?: boolean;
    disablePreReleaseUpdateNotifications?: boolean;
    console_notifications?: TelemetryNotificationsState;
    onboardingShown?: boolean;
  }>;
  hasura_uuid: string;
};

export const defaultConsoleState: ConsoleState = {
  console_opts: null,
  hasura_uuid: '',
};

export default defaultConsoleState;
