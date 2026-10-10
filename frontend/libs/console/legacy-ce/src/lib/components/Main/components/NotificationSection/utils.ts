import { versionGT, getLSItem } from '@hasura/shared/utils';
import {
  ConsoleState,
  ConsoleNotification,
  ConsoleScope,
  NotificationDate,
  NotificationScope,
} from '@hasura/metadata/api';

import { LS_KEYS } from '@hasura/shared/types';

export const getDateString = (date: NotificationDate) => {
  if (!date) {
    return '';
  }
  try {
    const dateString = new Date(date).toDateString().split(' ');
    const month = dateString[1].toUpperCase();
    const day = dateString[2];
    return `${month} ${day}`;
  } catch {
    return '';
  }
};

// toShowNotification is used to help render the valid notifications on the screen
export const toShowNotification = (
  consoleScope: ConsoleScope,
  notificationScope?: NotificationScope,
  isSpecial?: boolean,
) => {
  if (isSpecial) {
    return true;
  }

  if (notificationScope) {
    if (
      notificationScope.includes(consoleScope) ||
      notificationScope.indexOf(consoleScope) > -1
    ) {
      return true;
    }
  }

  return false;
};

export const errorNotification: ConsoleNotification = {
  subject: 'Error in Fetching Notifications',
  created_at: Date.now(),
  content:
    'There was an error in fetching notifications. Try again in some time.',
  type: 'error',
  start_date: null,
  priority: 1,
  expiry_date: null,
};

export const checkIsRead = (prevRead?: string | string[], id?: number) => {
  if (prevRead === 'all' || prevRead === 'default' || prevRead === 'error') {
    return true;
  }
  if (!prevRead || !id) {
    return false;
  }
  return prevRead.indexOf(`${id}`) !== -1;
};

export const checkVersionUpdate = (
  latestStable: string,
  latestPreRelease: string,
  serverVersion: string,
  console_opts: ConsoleState | null,
): [boolean, string] => {
  if (!console_opts || !latestStable || !latestPreRelease || !serverVersion) {
    return [false, ''];
  }

  const allowPreReleaseNotifications =
    !console_opts || !console_opts.disablePreReleaseUpdateNotifications;

  let latestServerVersionToCheck = latestStable;
  if (
    allowPreReleaseNotifications &&
    versionGT(latestPreRelease, latestStable)
  ) {
    latestServerVersionToCheck = latestPreRelease;
  }

  try {
    const lastUpdateCheckClosed = getLSItem(
      LS_KEYS.versionUpdateCheckLastClosed,
    );
    if (
      lastUpdateCheckClosed !== latestServerVersionToCheck ||
      serverVersion !== latestServerVersionToCheck
    ) {
      const isUpdateAvailable = versionGT(
        latestServerVersionToCheck,
        serverVersion,
      );

      if (isUpdateAvailable) {
        return [
          latestServerVersionToCheck.length > 0,
          latestServerVersionToCheck,
        ];
      }
    }
  } catch {
    return [false, ''];
  }
  return [false, ''];
};
