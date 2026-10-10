import { hasuraToast } from '@hasura/shared/ui';
import {
  CatalogState,
  ConsoleState,
  fetchCatalogState,
  setCatalogState as updateCatalogState,
  useErrorNotification,
} from '@hasura/metadata/api';
import { useAppContext, useAuthContext } from '@hasura/shared/context';
import { useTelemetryStore } from './store';

const useCatalogState = () => {
  const { endpoints, envVars } = useAppContext();
  const { hasuraUserId, getHeaders } = useAuthContext();
  const { consoleState, hasuraUuid, setCatalogState } = useTelemetryStore();
  const showErrorNotification = useErrorNotification();

  const userType = hasuraUserId || 'admin';

  const refreshCatalogState = async (): Promise<CatalogState> => {
    try {
      const headers = await getHeaders();
      const results = await fetchCatalogState(endpoints.metadata, headers);

      if (
        !results?.console_state ||
        (results.console_state.console_notifications &&
          !results.console_state.console_notifications[userType])
      ) {
        results.console_state = {
          console_notifications: {
            ...(results.console_state?.console_notifications ?? {}),
            [userType]: {
              read: [],
              date: null,
              showBadge: true,
            },
          },
        };
      }

      setCatalogState(results);

      return results;
    } catch (err) {
      console.error('failed to fetch catalog state', err);
      return {
        console_state: {
          ...consoleState,
          console_notifications: {
            ...consoleState?.console_notifications,
            [userType]: {
              read: [],
              date: null,
              showBadge: true,
            },
          },
        },
        id: hasuraUuid ?? '',
      };
    }
  };

  const updateConsoleState = async (state: ConsoleState) => {
    const headers = await getHeaders();
    await updateCatalogState(endpoints.metadata, state, headers);
    await refreshCatalogState();
  };

  const setOnboardingCompleted = () => {
    return updateConsoleState({
      ...consoleState,
      onboardingShown: true,
    })
      .then(() => {
        // the success notification won't be shown on cloud
        const isCloudContext = envVars.consoleType !== 'cloud';
        if (!isCloudContext) {
          hasuraToast({
            type: 'success',
            title: 'Success',
            message: 'Dismissed console onboarding',
          });
        }
      })
      .catch((err) => {
        showErrorNotification({
          title: 'Failed to update console onboarding status',
          error: err,
        });
      });
  };

  const setPreReleaseNotificationOptOut = () => {
    return updateConsoleState({
      ...consoleState,
      disablePreReleaseUpdateNotifications: true,
    })
      .then(() => {
        // the success notification won't be shown on cloud
        const isCloudContext = envVars.consoleType !== 'cloud';
        if (!isCloudContext) {
          hasuraToast({
            type: 'success',
            title: 'Success',
            message: 'Opted out of pre-release version release notifications',
          });
        }
      })
      .catch((err) => {
        showErrorNotification({
          title: 'Failed to opt out',
          error: err,
        });
      });
  };

  return {
    refreshCatalogState,
    updateConsoleState,
    setOnboardingCompleted,
    setPreReleaseNotificationOptOut,
  };
};

export default useCatalogState;
