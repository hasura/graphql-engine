import { ConsoleNotification } from '../components/Main/ConsoleNotification';
import { NotificationsState } from '../types';

export const isUpdateIDsEqual = (
  arr1: ConsoleNotification[],
  arr2: NotificationsState['read']
) => {
  return arr1.every(notif => {
    if (!notif.id) {
      return false;
    }
    return arr2.includes(`${notif.id}`);
  });
};
