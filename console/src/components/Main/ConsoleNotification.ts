import { AllowedBadges } from '../UIKit/atoms/Badge';

export type NotificationDate = string | number | Date | null;

export type ConsoleScope = 'OSS' | 'CLOUD' | 'PRO';
export type NotificationScope =
  | 'OSS'
  | 'CLOUD'
  | 'PRO'
  | 'OSS/CLOUD'
  | 'OSS/PRO'
  | 'CLOUD/PRO'
  | 'CLOUD/OSS'
  | 'PRO/OSS'
  | 'PRO/CLOUD'
  | 'PRO/CLOUD/OSS'
  | 'CLOUD/OSS/PRO'
  | 'OSS/CLOUD/PRO';

export type ConsoleNotification = {
  id?: number;
  subject: string;
  content: string;
  type: AllowedBadges | null;
  is_active?: boolean;
  created_at?: NotificationDate;
  external_link?: string | null;
  start_date?: NotificationDate;
  priority?: number;
  expiry_date?: NotificationDate;
  scope?: NotificationScope;
};

export const defaultNotification: ConsoleNotification = {
  subject: 'No updates available at the moment',
  created_at: Date.now(),
  content: "You're all caught up!",
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
  type: 'error',
  start_date: null,
  priority: 1,
  expiry_date: null,
};
