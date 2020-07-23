import { AllowedBadges } from '../UIKit/atoms/Badge';

export type NotificationDate = string | number | Date | null;

export type ConsoleNotification = {
  id?: number;
  subject: string;
  content: string;
  type: AllowedBadges | null;
  is_active?: boolean;
  created_at: NotificationDate;
  external_link?: string | null;
  start_date: NotificationDate;
  priority: number;
  expiry_date: NotificationDate;
};

export const defaultNotification: ConsoleNotification = {
  subject: '',
  created_at: Date.now(),
  content:
    "You're all caught up! \n There are no updates available at this point in time.",
  type: null,
  start_date: Date.now(),
  priority: 1,
  expiry_date: null,
};

export const errorNotification: ConsoleNotification = {
  subject: 'Error in Fetching Notifications',
  created_at: Date.now(),
  content:
    'There was an error in fetching notifications. Try again in some time.',
  type: null,
  start_date: null,
  priority: 1,
  expiry_date: null,
};
