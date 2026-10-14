import React from 'react';
import { FaBell, FaTimes } from 'react-icons/fa';
import { Flex, Heading, Popover } from '@radix-ui/themes';
import { Button, IconButton, Text, Separator } from '@hasura/shared/ui';
import styles from '../../Main.module.scss';
import { checkStableVersion } from '@hasura/shared/utils';
import { getReadAllNotificationsState } from '../../utils';
import {
  linkStyle,
  activeLinkStyle,
  itemContainerStyle,
} from '../HeaderNavItem';
import clsx from 'clsx';
import { getConsoleScope } from '../../../../telemetry/utils';
import {
  useCatalogState,
  useConsoleNotifications,
  useTelemetryStore,
} from '../../../../telemetry';
import { useAppContext } from '@hasura/shared/context';
import { useAuthContext } from '@hasura/shared/context';
import { checkIsRead, checkVersionUpdate } from './utils';
import { NotificationsState } from '@hasura/metadata/api';
import NotificationsListItem, {
  NotificationsListItemProps,
} from './NotificationsListItem';
import ToReadBadge from './ToReadBadge';
import ViewMoreOptions from './ViewMoreOptions';

const DEFAULT_SHOWN_COUNT = 20;

function useNotificationsPagination(totalNotificationsCount: number) {
  const [shownCount, setShownCount] = React.useState(DEFAULT_SHOWN_COUNT);

  const showMore = () => {
    if (shownCount < totalNotificationsCount) {
      const diff = totalNotificationsCount - shownCount;
      if (diff > DEFAULT_SHOWN_COUNT) {
        setShownCount((num) => num + DEFAULT_SHOWN_COUNT);
        return;
      }
      setShownCount((num) => num + diff);
    }
  };

  const reset = () => {
    setShownCount(DEFAULT_SHOWN_COUNT);
  };

  return { showMore, reset, shownCount };
}

const HasuraNotifications = () => {
  const { serverVersion, latestServerVersion } = useAppContext();
  const { hasuraUserId } = useAuthContext();
  const { refreshCatalogState, setPreReleaseNotificationOptOut } =
    useCatalogState();
  const { updateConsoleNotificationsState, getConsoleNotification } =
    useConsoleNotifications(refreshCatalogState);
  const { consoleState, notifications } = useTelemetryStore();
  const consoleId = window.__env?.consoleId;
  const consoleNotificationsLength = notifications?.length || 0;
  const consoleScope = getConsoleScope(serverVersion, consoleId);

  const pagination = useNotificationsPagination(notifications.length);

  const [opened, updateOpenState] = React.useState(false);

  const userType = hasuraUserId || 'admin';

  const previouslyReadState = React.useMemo(
    () =>
      consoleState?.console_notifications &&
      consoleState?.console_notifications[userType]?.read,
    [consoleState?.console_notifications, userType],
  );
  const showBadge = React.useMemo(
    () =>
      consoleState?.console_notifications &&
      consoleState?.console_notifications[userType]?.showBadge,
    [consoleState?.console_notifications, userType],
  );

  // `getConsoleNotification` is recreated on every render, so capture the
  // mount-time version in a ref and call it exactly once on mount (the original
  // `[]` intent) without re-running on every render.
  const getConsoleNotificationRef = React.useRef(getConsoleNotification);
  React.useEffect(() => {
    getConsoleNotificationRef.current();
  }, []);

  // The latest version and whether to surface a new-version update are fully
  // derived from the server versions + console state, so compute them during
  // render instead of mirroring them into state from an effect.
  const [versionUpdateCheck, latestReleasedVersion] = React.useMemo(
    () =>
      checkVersionUpdate(
        latestServerVersion.latest,
        latestServerVersion.prerelease,
        serverVersion,
        consoleState,
      ),
    [latestServerVersion, consoleState, serverVersion],
  );
  const latestVersion = latestReleasedVersion || serverVersion;
  const displayNewVersionUpdate = Boolean(versionUpdateCheck);

  const fixedVersion = React.useMemo(() => {
    const vulnerableVersionsMapping: Record<string, string> = {
      'v1.2.0-beta.5': 'v1.2.1',
      'v1.2.0': 'v1.2.1',
    };

    return vulnerableVersionsMapping[serverVersion] || '';
  }, [serverVersion]);

  // once mark all as read is clicked
  const computeReadNumber = () => {
    let readNumber = consoleNotificationsLength;

    if (
      previouslyReadState === 'all' ||
      previouslyReadState === 'default' ||
      previouslyReadState === 'error'
    ) {
      readNumber = 0;
    }

    if (Array.isArray(previouslyReadState)) {
      readNumber -= previouslyReadState.length;
    }

    return readNumber;
  };

  // `numberNotifications` is reset whenever any of these inputs change, but it is
  // also decremented locally by `onClickUpdate` between resets, so it cannot be
  // purely derived. Use the React "adjust state during render" pattern instead
  // of synchronously setting state inside an effect.
  const [numberNotifications, updateNumberNotifications] =
    React.useState(computeReadNumber);
  const [prevNotificationInputs, setPrevNotificationInputs] = React.useState({
    consoleNotificationsLength,
    displayNewVersionUpdate,
    userType,
    previouslyReadState,
    fixedVersion,
  });
  if (
    prevNotificationInputs.consoleNotificationsLength !==
      consoleNotificationsLength ||
    prevNotificationInputs.displayNewVersionUpdate !==
      displayNewVersionUpdate ||
    prevNotificationInputs.userType !== userType ||
    prevNotificationInputs.previouslyReadState !== previouslyReadState ||
    prevNotificationInputs.fixedVersion !== fixedVersion
  ) {
    setPrevNotificationInputs({
      consoleNotificationsLength,
      displayNewVersionUpdate,
      userType,
      previouslyReadState,
      fixedVersion,
    });
    updateNumberNotifications(computeReadNumber());
  }

  const onClickUpdate = React.useCallback(
    (id?: number) => {
      updateNumberNotifications((prev) => prev - 1);

      if (!id) {
        return;
      }

      if (
        previouslyReadState === 'all' ||
        previouslyReadState === 'default' ||
        previouslyReadState === 'error' ||
        !previouslyReadState
      ) {
        return;
      }

      if (!previouslyReadState.includes(`${id}`)) {
        updateConsoleNotificationsState({
          read: [...previouslyReadState, `${id}`],
          date: new Date().toISOString(),
          showBadge: false,
        });
      }
    },
    [previouslyReadState, updateConsoleNotificationsState],
  );

  const optOutCallback = React.useCallback(() => {
    setPreReleaseNotificationOptOut();
  }, [setPreReleaseNotificationOptOut]);

  const onClickMarkAllAsRead = () => {
    const readAllState = getReadAllNotificationsState();
    updateConsoleNotificationsState(readAllState);
    pagination.reset();
    // to clear the beta-version update if you mark all as read
    if (!checkStableVersion(latestVersion) && displayNewVersionUpdate) {
      optOutCallback();
    }
  };

  const onClickOutside = () => {
    updateOpenState(false);
  };

  const onClickNotificationButton = () => {
    if (showBadge) {
      if (consoleState?.console_notifications) {
        const updatedState = consoleState.console_notifications[userType]?.date
          ? {
              ...consoleState.console_notifications[userType],
              showBadge: false,
            }
          : {
              ...consoleState.console_notifications[userType],
              date: new Date().toISOString(),
              showBadge: false,
            };
        updateConsoleNotificationsState(updatedState as NotificationsState);
      }
    }
    if (!opened) {
      updateOpenState(true);
    }
  };

  React.useEffect(() => {
    if (!opened) {
      pagination.reset();
    }
  }, [opened, pagination]);

  const dataShown = React.useMemo<Array<NotificationsListItemProps>>(() => {
    return [
      fixedVersion && {
        kind: 'security',
        props: { fixedVersion },
      },
      displayNewVersionUpdate && {
        kind: 'version-update',
        props: {
          latestVersion,
          optOutCallback,
          onClick: onClickUpdate,
        },
      },
      ...notifications.slice(0, pagination.shownCount).map((payload: any) => ({
        kind: 'default',
        props: {
          id: payload.id,
          onClick: onClickUpdate,
          is_read: checkIsRead(previouslyReadState, payload.id),
          consoleScope,
          ...payload,
        },
      })),
    ].filter((x): x is NotificationsListItemProps => Boolean(x));
  }, [
    notifications,
    consoleScope,
    displayNewVersionUpdate,
    fixedVersion,
    latestVersion,
    optOutCallback,
    onClickUpdate,
    pagination.shownCount,
    previouslyReadState,
  ]);

  const shouldDisplayViewMore =
    consoleNotificationsLength > 20 &&
    pagination.shownCount !== consoleNotificationsLength;

  return (
    <Popover.Root onOpenChange={updateOpenState} open={opened}>
      <Popover.Trigger>
        <div className={itemContainerStyle}>
          <div
            className={clsx(
              'dropdown-toggle',
              linkStyle,
              opened ? `${styles.opened} ${activeLinkStyle}` : '',
            )}
            aria-expanded="false"
            onClick={onClickNotificationButton}
          >
            <span className="relative">
              <FaBell className={styles.bellIcon} />
              <ToReadBadge
                numberNotifications={numberNotifications}
                show={showBadge || !!fixedVersion}
              />
            </span>
          </div>
        </div>
      </Popover.Trigger>
      <Popover.Content size="3" className="w-[460px] overflow-hidden">
        <div>
          <Flex align="center" justify="between">
            <Heading size="2" className="ml-2">
              Notifications{' '}
              {numberNotifications > 0 ? `(${numberNotifications})` : ''}
            </Heading>
            <Flex gap="4" align="center">
              <Button
                size="2"
                title="Mark all as read"
                onClick={onClickMarkAllAsRead}
                disabled={!numberNotifications || !notifications.length}
                variant="ghost"
                color="gray"
              >
                <Text size="1" className="uppercase" weight="bold">
                  Mark all as read
                </Text>
              </Button>

              <Popover.Close onClick={onClickOutside}>
                <IconButton size="2" variant="ghost" color="gray">
                  <FaTimes />
                </IconButton>
              </Popover.Close>
            </Flex>
          </Flex>
          <Separator className="my-2 w-full!" />
          <div
            className="overflow-hidden"
            style={{
              maxHeight: 'calc(100vh - 112px)',
            }}
          >
            {dataShown.length > 0 &&
              dataShown.map((payload, i) => (
                <NotificationsListItem key={i} {...payload} />
              ))}
            {shouldDisplayViewMore && (
              <ViewMoreOptions
                onClickViewMore={pagination.showMore}
                readAll={previouslyReadState === 'all'}
              />
            )}
          </div>
        </div>
      </Popover.Content>
    </Popover.Root>
  );
};

export default HasuraNotifications;
