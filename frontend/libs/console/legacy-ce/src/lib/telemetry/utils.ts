import type {
  ConsoleNotification,
  ConsoleScope,
  NotificationsReadState,
} from '@hasura/metadata/api';
import { getLSItem } from '@hasura/shared/utils';
import { LS_KEYS } from '@hasura/shared/types';

export const defaultNotification: ConsoleNotification = {
  subject: 'No updates available at the moment',
  created_at: Date.now(),
  content: "You're all caught up!",
  type: null,
  start_date: Date.now(),
  priority: 1,
  expiry_date: null,
};

export const isUpdateIDsEqual = (
  arr1: ConsoleNotification[],
  arr2: NotificationsReadState,
) => {
  if (Array.isArray(arr2) && arr1.length) {
    return arr1.every((notif) => {
      if (!notif.id) {
        return false;
      }
      return arr2.includes(`${notif.id}`);
    });
  }

  return false;
};

export const getConsoleScope = (
  serverVersion: string,
  consoleID: string | null | undefined,
): ConsoleScope => {
  if (!consoleID) {
    return 'OSS';
  }

  if (serverVersion?.includes('cloud')) {
    return 'CLOUD';
  }

  // EE classic edition no longer has the pro suffix.
  return 'PRO';
};

export const getLastSeenNotifications = (): number | undefined => {
  try {
    const rawNotificationLastSeen = getLSItem(LS_KEYS.notificationsLastSeen);
    return rawNotificationLastSeen
      ? JSON.parse(rawNotificationLastSeen)
      : undefined;
  } catch {
    return undefined;
  }
};
