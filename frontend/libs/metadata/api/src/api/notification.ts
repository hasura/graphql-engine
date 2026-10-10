import { GraphQLError } from 'graphql';
import { requestJson } from '@hasura/shared/utils';

export type NotificationDate = string | number | Date | null;
export type NotificationsReadState = 'all' | 'default' | 'error' | string[];

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

export type NotificationsState = {
  read: NotificationsReadState;
  date: string | null; // ISO String
  showBadge: boolean;
};

export type ConsoleNotification = {
  id?: number;
  subject: string;
  content: string;
  type: string | null;
  is_active?: boolean;
  created_at?: NotificationDate;
  external_link?: string | null;
  start_date?: NotificationDate;
  priority?: number;
  expiry_date?: NotificationDate;
  scope?: NotificationScope;
};

const getConsoleNotificationQuery = (
  time: Date | string | number,
  userType?: ConsoleScope,
) => {
  let consoleUserScopeVar = `%${userType}%`;
  if (!userType) {
    consoleUserScopeVar = '%OSS%';
  }

  const query = `query fetchNotifications($currentTime: timestamptz, $userScope: String) {
    console_notifications(
      where: {start_date: {_lte: $currentTime}, scope: {_ilike: $userScope}, _or: [{expiry_date: {_gte: $currentTime}}, {expiry_date: {_eq: null}}]},
      order_by: {priority: asc_nulls_last, start_date: desc}
    ) {
      content
      created_at
      external_link
      expiry_date
      id
      is_active
      priority
      scope
      start_date
      subject
      type
    }
  }`;

  const variables = {
    userScope: consoleUserScopeVar,
    currentTime: time,
  };

  return { query, variables };
};

export const fetchConsoleNotifications = async (
  url: string,
  consoleScope: ConsoleScope,
): Promise<ConsoleNotification[]> => {
  const now = new Date().toISOString();
  const payload = getConsoleNotificationQuery(now, consoleScope);
  const options = {
    body: JSON.stringify(payload),
    method: 'POST',
    headers: {
      // temp. change until Auth is added
      'x-hasura-role': 'user',
    },
  };

  return requestJson<{
    data?: {
      console_notifications: ConsoleNotification[];
    };
    errors?: GraphQLError[];
  }>(url, options).then((response) => {
    if (response.errors?.length) {
      throw new GraphQLError(response.errors[0].message, response.errors[0]);
    }

    return response.data?.console_notifications ?? [];
  });
};
