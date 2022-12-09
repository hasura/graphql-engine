import { ConsoleNotification } from '../components/Main/ConsoleNotification';
import { NotificationsState } from './state';

export const isUpdateIDsEqual = (
  arr1: ConsoleNotification[],
  arr2: NotificationsState['read']
) => {
  if (Array.isArray(arr2) && arr1.length) {
    return arr1.every(notif => {
      if (!notif.id) {
        return false;
      }
      return arr2.includes(`${notif.id}`);
    });
  }

  return false;
};
