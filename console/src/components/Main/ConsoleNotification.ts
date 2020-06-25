import { AllowedBadges } from '../UIKit/atoms/Badge';

export type ConsoleNotification = {
  subject: string;
  created_at: string | number | Date;
  content: string;
  type: AllowedBadges;
  id?: number;
  is_active?: boolean;
  external_link?: string;
};

export const defaultNotification: ConsoleNotification = {
  subject: '',
  created_at: Date.now(),
  content:
    "You're all caught up! \n There are no updates available at this point in time.",
  type: 'no updates',
};

export const errorNotification: ConsoleNotification = {
  subject: 'Error in Fetching Notifications',
  created_at: Date.now(),
  content:
    'There was an error in fetching notifications. Try again in some time.',
  type: 'error',
};
