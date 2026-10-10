import { NotificationsState } from './notification';
import { requestJson } from '@hasura/shared/utils';

export type ConsoleNotificationsState = Record<string, NotificationsState>;
export type ConsoleState = {
  disablePreReleaseUpdateNotifications?: boolean;
  console_notifications?: ConsoleNotificationsState;
  onboardingShown?: boolean;
};

export type CatalogState = {
  console_state: ConsoleState;
  id: string;
};

export const fetchCatalogState = (
  endpoint: string,
  headers: Record<string, string>,
): Promise<CatalogState> => {
  const options = {
    method: 'POST',
    headers,
    body: JSON.stringify({
      type: 'get_catalog_state',
      args: {},
    }),
  };

  return requestJson<CatalogState>(endpoint, options);
};

export type SetCatalogStateOutput = { message: 'success' };

export const setCatalogState = (
  endpoint: string,
  state: ConsoleState,
  headers: Record<string, string>,
): Promise<SetCatalogStateOutput> => {
  const options = {
    method: 'POST',
    headers,
    body: JSON.stringify({
      type: 'set_catalog_state',
      args: {
        type: 'console',
        state,
      },
    }),
  };

  return requestJson<SetCatalogStateOutput>(endpoint, options);
};
