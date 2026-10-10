import {
  fetchConsoleNotifications,
  NotificationsReadState,
  NotificationsState,
  CatalogState,
  ConsoleState,
  setCatalogState as updateCatalogState,
} from '@hasura/metadata/api';
import { getLSItem, setLSItem } from '@hasura/shared/utils';
import {
  defaultNotification,
  getConsoleScope,
  isUpdateIDsEqual,
} from './utils';
import { useTelemetryStore } from './store';
import { useAuthContext, useAppContext } from '@hasura/shared/context';
import { LS_KEYS } from '@hasura/shared/types';

const useConsoleNotifications = (
  refreshCatalogState: () => Promise<CatalogState>,
) => {
  const { serverVersion, endpoints, isProduction } = useAppContext();
  const { hasuraUserId, getHeaders } = useAuthContext();
  const { consoleState, notifications } = useTelemetryStore();

  const userType = hasuraUserId || 'admin';
  const notificationUrl = !isProduction
    ? endpoints.consoleNotificationsStg
    : endpoints.consoleNotificationsProd;

  const updateConsoleNotificationsState = async (
    updatedState: NotificationsState,
  ) => {
    const catalogState = await refreshCatalogState();
    if (!catalogState) {
      return;
    }

    const { console_state: current_console_state } = catalogState;
    let composedUpdatedState: ConsoleState = {
      ...current_console_state,
      console_notifications: {
        ...current_console_state?.console_notifications,
      },
    };

    const userType = hasuraUserId || 'admin';
    const dbReadState =
      current_console_state?.console_notifications?.[userType]?.read;
    let combinedReadState: NotificationsReadState = [];

    if (!dbReadState || dbReadState === 'default' || dbReadState === 'error') {
      composedUpdatedState = {
        ...current_console_state,
        console_notifications: {
          ...current_console_state?.console_notifications,
          [userType]: updatedState,
        },
      };
    } else if (dbReadState === 'all') {
      if (updatedState.read === 'all') {
        composedUpdatedState = {
          ...current_console_state,
          console_notifications: {
            ...current_console_state?.console_notifications,
            [userType]: {
              read: 'all',
              date: updatedState.date,
              showBadge: false,
            },
          },
        };
      } else {
        composedUpdatedState = {
          ...current_console_state,
          console_notifications: {
            ...current_console_state?.console_notifications,
            [userType]: updatedState,
          },
        };
      }
    } else {
      if (typeof updatedState.read === 'string') {
        combinedReadState = updatedState.read;
      } else if (Array.isArray(updatedState.read)) {
        // this is being done to ensure that there is a consistency between the read
        // state of the users and the data present in the DB
        combinedReadState = dbReadState
          .concat(updatedState.read)
          .reduce((acc: string[], val: string) => {
            if (!acc.includes(val)) {
              return [...acc, val];
            }
            return acc;
          }, []);
      }

      composedUpdatedState = {
        ...current_console_state,
        console_notifications: {
          ...current_console_state?.console_notifications,
          [userType]: {
            ...updatedState,
            read: combinedReadState,
          },
        },
      };
    }

    if (
      notifications &&
      Array.isArray(notifications) &&
      Array.isArray(combinedReadState)
    ) {
      if (isUpdateIDsEqual(notifications, combinedReadState)) {
        composedUpdatedState = {
          ...current_console_state,
          console_notifications: {
            ...current_console_state?.console_notifications,
            [userType]: {
              read: 'all',
              showBadge: false,
              date: updatedState.date,
            },
          },
        };
      }
    }

    const headers = await getHeaders();
    return updateCatalogState(notificationUrl, composedUpdatedState, headers)
      .then(refreshCatalogState)
      .catch((err) => {
        console.error('failed to update catalog state', err);
      });
  };

  const getConsoleNotification = async () => {
    const now = new Date().toISOString();
    try {
      let toShowBadge = true;
      let previousRead: NotificationsState['read'] = [];
      const consoleId = window.__env.consoleId;
      const consoleScope = getConsoleScope(serverVersion, consoleId);

      if (consoleState?.console_notifications?.[userType].date) {
        toShowBadge = consoleState.console_notifications[userType].showBadge;
        previousRead = consoleState.console_notifications[userType].read;
      }

      const fetchedData = await fetchConsoleNotifications(
        notificationUrl,
        consoleScope,
      ).catch((err) => {
        console.error('failed to fetch console notifications', err);
        return [];
      });
      const rawNotificationLastSeen = getLSItem(LS_KEYS.notificationsLastSeen);
      let lastSeenNotifications = 0;

      try {
        lastSeenNotifications = rawNotificationLastSeen
          ? JSON.parse(rawNotificationLastSeen)
          : undefined;
      } catch {
        // Corrupt/legacy value in localStorage: fall back to `undefined`
        // (treated as "never seen"), which is already the initial value.
      }

      if (!fetchedData.length) {
        updateConsoleNotificationsState({
          read: 'default',
          date: now,
          showBadge: false,
        });

        setLSItem(
          LS_KEYS.notificationsLastSeen,
          JSON.stringify(lastSeenNotifications),
        );

        return [defaultNotification];
      }

      if (previousRead) {
        if (!consoleState?.console_notifications) {
          await updateConsoleNotificationsState({
            read: [],
            date: now,
            showBadge: true,
          });
        } else {
          let newReadValue;
          if (previousRead === 'default' || previousRead === 'error') {
            newReadValue = [];
            toShowBadge = false;
          } else if (previousRead === 'all') {
            // we don't have a record of the IDs that were marked as read previously
            newReadValue = [];
            toShowBadge = true;
          } else {
            newReadValue = previousRead;
            if (
              previousRead.length &&
              lastSeenNotifications >= fetchedData.length
            ) {
              toShowBadge = false;
            } else if (lastSeenNotifications < fetchedData.length) {
              toShowBadge = true;
            }
          }

          await updateConsoleNotificationsState({
            read: newReadValue,
            date: consoleState.console_notifications[userType].date,
            showBadge: toShowBadge,
          });
        }
      }

      // update/set the lastSeen value upon data is set
      if (
        !lastSeenNotifications ||
        lastSeenNotifications !== fetchedData.length
      ) {
        setLSItem(
          LS_KEYS.notificationsLastSeen,
          JSON.stringify(fetchedData.length),
        );
      }

      return fetchedData;
    } catch (err) {
      await updateConsoleNotificationsState({
        read: 'error',
        date: now,
        showBadge: false,
      });

      throw err;
    }
  };

  return {
    getConsoleNotification,
    updateConsoleNotificationsState,
  };
};

export default useConsoleNotifications;
