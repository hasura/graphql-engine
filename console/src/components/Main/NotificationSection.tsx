import React, { ComponentProps } from 'react';
import { connect, ConnectedProps } from 'react-redux';

import { Box, Flex, Heading, Text, Badge } from '../UIKit/atoms';
import {
  ConsoleNotification,
  NotificationDate,
  NotificationScope,
  ConsoleScope,
} from './ConsoleNotification';
import styles from './Main.scss';
import useOnClickOutside from '../../hooks/useOnClickOutside';
import { ReduxState } from '../../types';
import { versionGT, checkStableVersion } from '../../helpers/versionUtils';
import ToolTip from '../Common/Tooltip/Tooltip';
import {
  setPreReleaseNotificationOptOutInDB,
  updateConsoleNotificationsState,
} from '../../telemetry/Actions';
import Button from '../Common/Button';
import {
  getReadAllNotificationsState,
  getConsoleScope,
  getUserType,
} from './utils';
import { Nullable } from '../Common/utils/tsUtils';
import { mapDispatchToPropsEmpty } from '../Common/utils/reactUtils';
import { HASURA_COLLABORATOR_TOKEN } from '../../constants';
import { StyledText } from '../UIKit/atoms/Typography/Typography';
import { LS_KEYS } from '../../utils/localStorage';
import { ConsoleState, NotificationsState } from '../../telemetry/state';

const getDateString = (date: NotificationDate) => {
  if (!date) {
    return '';
  }
  try {
    const dateString = new Date(date).toDateString().split(' ');
    const month = dateString[1].toUpperCase();
    const day = dateString[2];
    return `${month} ${day}`;
  } catch {
    return '';
  }
};

// toShowNotification is used to help render the valid notifications on the screen
const toShowNotification = (
  consoleScope: ConsoleScope,
  notificationScope?: NotificationScope,
  isSpecial?: boolean
) => {
  if (isSpecial) {
    return true;
  }

  if (notificationScope) {
    if (
      notificationScope.includes(consoleScope) ||
      notificationScope.indexOf(consoleScope) > -1
    ) {
      return true;
    }
  }

  return false;
};

interface UpdateProps extends ConsoleNotification {
  onClick?: (id?: number) => void;
  is_read?: boolean;
  consoleScope: ConsoleScope;
  latestVersion?: string;
  stable?: boolean;
  isSpecial?: boolean;
}

const Notification: React.FC<UpdateProps> = ({
  subject,
  content,
  type,
  is_active = true,
  onClick,
  is_read,
  isSpecial,
  ...props
}) => {
  const [currentReadState, updateReadState] = React.useState(is_read);

  React.useEffect(() => {
    if (is_read) {
      updateReadState(true);
      return;
    }
    updateReadState(false);
  }, [is_read]);

  const onClickNotification = () => {
    if (!currentReadState) {
      updateReadState(true);
    }
    if (onClick) {
      onClick(props.id);
    }
  };

  if (!is_active) {
    return null;
  }

  if (!toShowNotification(props.consoleScope, props.scope, isSpecial)) {
    return null;
  }

  const isUpdateNotification =
    type === 'beta update' || type === 'version update';
  const updateContainerClass = isUpdateNotification
    ? styles.updateStyleBox
    : styles.updateBox;

  return (
    <Box
      className={`${updateContainerClass} ${
        !currentReadState ? styles.unread : styles.read
      }`}
      onClick={onClickNotification}
    >
      {!isUpdateNotification ? (
        <div
          className={`${styles.unreadDot} ${
            currentReadState ? styles.hideDot : ''
          }`}
        />
      ) : (
        <span
          className={`${styles.unreadStar} ${
            currentReadState ? styles.hideStar : ''
          }`}
          role="img"
          aria-label="star emoji"
        >
          ⭐️
        </span>
      )}
      <Flex width="100%">
        <Flex pl={32} pr={25} py={2} width="80%" flexDirection="column">
          <StyledText
            color="#717780"
            fontSize="10px"
            fontWeight="bold"
            lineHeight="12px"
            margin="0"
            paddingBottom="4px"
          >
            {props?.start_date ? getDateString(props.start_date) : null}
          </StyledText>
          <Heading
            as="h4"
            color="#1B2738"
            fontSize="14px"
            fontWeight="bold"
            lineHeight="153%"
            marginBottom="12px"
          >
            {subject}
          </Heading>
          <Text fontSize={15} fontWeight="normal">
            {content}
            <br />
            {props?.children ? props.children : null}
          </Text>
        </Flex>
        <Flex
          width="20%"
          alignItems="flex-end"
          justifyContent="flex-end"
          marginRight="24px"
          marginTop="10px"
          flexDirection="column"
        >
          <div className={`${styles.splitHalf}`}>
            {type ? <Badge type={type} mt="8px" /> : null}
          </div>
          <div className={`${styles.splitHalf} ${styles.linkPosition}`}>
            {props.external_link ? (
              <div className={styles.linkContainer}>
                <a
                  href={props.external_link}
                  className={styles.notificationExternalLink}
                  onClick={onClickNotification}
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  <i className={`fa fa-arrow-right ${styles.linkArrow}`} />
                </a>
              </div>
            ) : null}
          </div>
        </Flex>
      </Flex>
    </Box>
  );
};

type PreReleaseProps = {
  optOutCallback: () => void;
};

const PreReleaseNote: React.FC<PreReleaseProps> = ({ optOutCallback }) => (
  <Flex pt={4}>
    <i>
      This is a pre-release version. Not recommended for production use.
      <br />
      <a href="#" onClick={optOutCallback}>
        Opt out of pre-release notifications
      </a>
      <ToolTip
        message="Only be notified about stable releases"
        placement="top"
      />
    </i>
  </Flex>
);

interface VersionUpdateNotificationProps extends PreReleaseProps {
  latestVersion: string;
  onClick: () => void;
}

const VersionUpdateNotification: React.FC<VersionUpdateNotificationProps> = ({
  latestVersion,
  optOutCallback,
  onClick,
}) => {
  const isStableRelease = checkStableVersion(latestVersion);
  const changeLogURL = `https://github.com/hasura/graphql-engine/releases${
    latestVersion ? `/tag/${latestVersion}` : ''
  }`;

  const handleClick = () => {
    window.localStorage.setItem(
      LS_KEYS.versionUpdateCheckLastClosed,
      latestVersion || ''
    );
    onClick();
  };

  return (
    <Notification
      subject="New Update Available!"
      type={isStableRelease ? 'version update' : 'beta update'}
      content={`Hey There! There's a new server version ${latestVersion} available.`}
      start_date={Date.now()}
      consoleScope="OSS"
      latestVersion={latestVersion}
      stable={isStableRelease}
      onClick={handleClick}
      isSpecial
    >
      <a href={changeLogURL} target="_blank" rel="noopener noreferrer">
        <span>View Changelog</span>
      </a>
      <span className={styles.middot}> &middot; </span>
      <a
        className={styles.updateLink}
        href="https://hasura.io/docs/latest/graphql/core/deployment/updating.html"
        target="_blank"
        rel="noopener noreferrer"
      >
        <span>Update Now</span>
      </a>
      {!isStableRelease && <PreReleaseNote optOutCallback={optOutCallback} />}
    </Notification>
  );
};

type VulnerableVersionProps = {
  fixedVersion: string;
};

const VulnerableVersionNotification: React.FC<VulnerableVersionProps> = ({
  fixedVersion,
}) => (
  <Notification
    type="security"
    subject="Security Vulnerability Located!"
    content={`This current server version has a security vulnerability. Please upgrade to ${fixedVersion} immediately.`}
    start_date={Date.now()}
    consoleScope="OSS"
    isSpecial
  >
    <a
      href={`https://github.com/hasura/graphql-engine/releases/tag/${fixedVersion}`}
      target="_blank"
      rel="noopener noreferrer"
    >
      <span>View Changelog</span>
    </a>
    <span className={styles.middot}> &middot; </span>
    <a
      className={styles.updateLink}
      href="https://hasura.io/docs/latest/graphql/core/deployment/updating.html"
      target="_blank"
      rel="noopener noreferrer"
    >
      <span>Update Now</span>
    </a>
  </Notification>
);

type ViewMoreProps = {
  onClickViewMore: () => void;
  readAll: boolean;
};

const ViewMoreOptions: React.FC<ViewMoreProps> = ({
  onClickViewMore,
  readAll,
}) => {
  const buttonText = !readAll
    ? 'View more notifications'
    : 'View older notifications';
  return (
    <Button className={styles.viewMoreNotifications} onClick={onClickViewMore}>
      {buttonText} &rarr;
    </Button>
  );
};

const checkIsRead = (prevRead?: string | string[], id?: number) => {
  if (prevRead === 'all' || prevRead === 'default' || prevRead === 'error') {
    return true;
  }
  if (!prevRead || !id) {
    return false;
  }
  return prevRead.indexOf(`${id}`) !== -1;
};

const checkVersionUpdate = (
  latestStable: string,
  latestPreRelease: string,
  serverVersion: string,
  console_opts: ConsoleState['console_opts']
): [boolean, string] => {
  if (!console_opts || !latestStable || !latestPreRelease || !serverVersion) {
    return [false, ''];
  }

  const allowPreReleaseNotifications =
    !console_opts || !console_opts.disablePreReleaseUpdateNotifications;

  let latestServerVersionToCheck = latestStable;
  if (
    allowPreReleaseNotifications &&
    versionGT(latestPreRelease, latestStable)
  ) {
    latestServerVersionToCheck = latestPreRelease;
  }

  try {
    const lastUpdateCheckClosed = window.localStorage.getItem(
      LS_KEYS.versionUpdateCheckLastClosed
    );
    if (
      lastUpdateCheckClosed !== latestServerVersionToCheck ||
      serverVersion !== latestServerVersionToCheck
    ) {
      const isUpdateAvailable = versionGT(
        latestServerVersionToCheck,
        serverVersion
      );

      if (isUpdateAvailable) {
        return [
          latestServerVersionToCheck.length > 0,
          latestServerVersionToCheck,
        ];
      }
    }
  } catch {
    return [false, ''];
  }
  return [false, ''];
};

type ToReadBadgeProps = {
  numberNotifications: number;
  show: Nullable<boolean>;
};

const ToReadBadge: React.FC<ToReadBadgeProps> = ({
  numberNotifications,
  show,
}) => {
  const showBadge = !show || numberNotifications <= 0 ? styles.hideBadge : '';
  let display = `${numberNotifications}`;
  if (numberNotifications > 20) {
    display = '20+';
  }
  return (
    <Flex
      className={`${styles.numBadge} ${showBadge}`}
      justifyContent="center"
      alignItems="center"
    >
      {display}
    </Flex>
  );
};

type NotificationsListItemProps =
  | {
      kind: 'version-update';
      props: {
        latestVersion: string;
        optOutCallback: () => void;
        onClick: () => void;
      };
    }
  | {
      kind: 'security';
      props: {
        fixedVersion: string;
      };
    }
  | {
      kind: 'default';
      props: ComponentProps<typeof Notification>;
    };

const NotificationsListItem = (props: NotificationsListItemProps) => {
  switch (props.kind) {
    case 'version-update':
      return <VersionUpdateNotification {...props.props} />;
    case 'security':
      return <VulnerableVersionNotification {...props.props} />;
    default:
      return <Notification {...props.props} />;
  }
};

const DEFAULT_SHOWN_COUNT = 20;
function useNotificationsPagination(totalNotificationsCount: number) {
  const [shownCount, setShownCount] = React.useState(DEFAULT_SHOWN_COUNT);

  const showMore = () => {
    if (shownCount < totalNotificationsCount) {
      const diff = totalNotificationsCount - shownCount;
      if (diff > DEFAULT_SHOWN_COUNT) {
        setShownCount(num => num + DEFAULT_SHOWN_COUNT);
        return;
      }
      setShownCount(num => num + diff);
    }
  };

  const reset = () => {
    setShownCount(DEFAULT_SHOWN_COUNT);
  };

  return { showMore, reset, shownCount };
}

const mapStateToProps = (state: ReduxState) => {
  return {
    consoleNotifications: state.main.consoleNotifications,
    latestPreReleaseServerVersion: state.main.latestPreReleaseServerVersion,
    latestStableServerVersion: state.main.latestStableServerVersion,
    serverVersion: state.main.serverVersion,
    console_opts: state.telemetry.console_opts,
    dataHeaders: state.tables.dataHeaders,
  };
};

type HasuraNotificationOwnProps = {
  isDropDownOpen: boolean;
  toggleDropDown: () => void;
  closeDropDown: () => void;
};

const HasuraNotifications: React.FC<
  HasuraNotificationsProps & HasuraNotificationOwnProps
> = ({
  consoleNotifications,
  toggleDropDown,
  closeDropDown,
  console_opts,
  latestStableServerVersion,
  latestPreReleaseServerVersion,
  serverVersion,
  dataHeaders,
  isDropDownOpen,
  dispatch,
}) => {
  // eslint-disable-next-line no-underscore-dangle
  const consoleId = window.__env.consoleId;
  const consoleNotificationsLength = consoleNotifications?.length || 0;
  const consoleScope = getConsoleScope(serverVersion, consoleId);

  const dropDownRef = React.useRef<HTMLDivElement>(null);
  const wrapperRef = React.useRef<HTMLDivElement>(null);

  const pagination = useNotificationsPagination(consoleNotifications.length);
  const [latestVersion, setLatestVersion] = React.useState(serverVersion);
  const [displayNewVersionUpdate, setDisplayNewVersionUpdate] = React.useState(
    false
  );

  const [opened, updateOpenState] = React.useState(false);
  const [numberNotifications, updateNumberNotifications] = React.useState(0);

  let userType = 'admin';

  const headerHasCollabToken = Object.keys(dataHeaders).find(
    header => header.toLowerCase() === HASURA_COLLABORATOR_TOKEN
  );

  if (headerHasCollabToken) {
    const collabToken = dataHeaders[headerHasCollabToken];
    userType = getUserType(collabToken);
  }

  const previouslyReadState = React.useMemo(
    () =>
      console_opts?.console_notifications &&
      console_opts?.console_notifications[userType]?.read,
    [console_opts?.console_notifications, userType]
  );
  const showBadge = React.useMemo(
    () =>
      console_opts?.console_notifications &&
      console_opts?.console_notifications[userType]?.showBadge,
    [console_opts?.console_notifications, userType]
  );

  React.useEffect(() => {
    const [versionUpdateCheck, latestReleasedVersion] = checkVersionUpdate(
      latestStableServerVersion,
      latestPreReleaseServerVersion,
      serverVersion,
      console_opts
    );

    setLatestVersion(latestReleasedVersion || serverVersion);

    if (versionUpdateCheck) {
      setDisplayNewVersionUpdate(true);
      return;
    }

    setDisplayNewVersionUpdate(false);
  }, [
    latestPreReleaseServerVersion,
    latestStableServerVersion,
    console_opts,
    serverVersion,
  ]);

  const fixedVersion = React.useMemo(() => {
    const vulnerableVersionsMapping: Record<string, string> = {
      'v1.2.0-beta.5': 'v1.2.1',
      'v1.2.0': 'v1.2.1',
    };

    return vulnerableVersionsMapping[serverVersion] || '';
  }, [serverVersion]);

  React.useEffect(() => {
    // once mark all as read is clicked
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

    updateNumberNotifications(readNumber);
  }, [
    consoleNotificationsLength,
    displayNewVersionUpdate,
    userType,
    previouslyReadState,
    fixedVersion,
  ]);

  const onClickUpdate = (id?: number) => {
    updateNumberNotifications(prev => prev - 1);

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
      dispatch(
        updateConsoleNotificationsState({
          read: [...previouslyReadState, `${id}`],
          date: new Date().toISOString(),
          showBadge: false,
        })
      );
    }
  };

  const optOutCallback = () => {
    closeDropDown();
    dispatch(setPreReleaseNotificationOptOutInDB());
  };

  const onClickMarkAllAsRead = () => {
    const readAllState = getReadAllNotificationsState();
    dispatch(updateConsoleNotificationsState(readAllState));
    pagination.reset();
    window.localStorage.setItem(
      'notifications:data',
      JSON.stringify(consoleNotifications)
    );
    // to clear the beta-version update if you mark all as read
    if (!checkStableVersion(latestVersion) && displayNewVersionUpdate) {
      optOutCallback();
    }
  };

  const onClickOutside = () => {
    updateOpenState(false);
    closeDropDown();
  };

  useOnClickOutside([dropDownRef, wrapperRef], onClickOutside);

  const onClickNotificationButton = () => {
    if (showBadge) {
      if (console_opts?.console_notifications) {
        let updatedState = {};
        if (console_opts.console_notifications[userType]?.date) {
          updatedState = {
            ...console_opts.console_notifications[userType],
            showBadge: false,
          };
        } else {
          updatedState = {
            ...console_opts.console_notifications[userType],
            date: new Date().toISOString(),
            showBadge: false,
          };
        }
        dispatch(
          updateConsoleNotificationsState(updatedState as NotificationsState)
        );
      }
    }
    if (!opened) {
      updateOpenState(true);
    }
    toggleDropDown();
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
      ...consoleNotifications
        .slice(0, pagination.shownCount)
        .map((payload: any) => ({
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
    consoleNotifications,
    consoleScope,
    displayNewVersionUpdate,
    fixedVersion,
    latestVersion,
    optOutCallback,
    pagination.shownCount,
    previouslyReadState,
  ]);

  const shouldDisplayViewMore =
    consoleNotificationsLength > 20 &&
    pagination.shownCount !== consoleNotificationsLength;

  return (
    <>
      <div
        className={`${styles.shareSection} ${styles.headerRightNavbarBtn} ${
          isDropDownOpen ? styles.opened : ''
        } dropdown-toggle`}
        aria-expanded="false"
        onClick={onClickNotificationButton}
        ref={wrapperRef}
      >
        <i className={`fa fa-bell ${styles.bellIcon}`} />
        <ToReadBadge
          numberNotifications={numberNotifications}
          show={showBadge || !!fixedVersion}
        />
      </div>
      <Box
        className={`dropdown-menu ${styles.consoleNotificationPanel}`}
        ref={dropDownRef}
      >
        <Flex
          alignItems="center"
          p={16}
          justifyContent="space-between"
          border="1px solid #f2f2f2"
        >
          <Flex alignItems="center" justifyContent="center">
            <Heading as="h4" color="#000" fontSize="12px" marginLeft="8px">
              Notifications{' '}
              {numberNotifications > 0 ? `(${numberNotifications})` : ''}
            </Heading>
          </Flex>
          <Button
            title="Mark all as read"
            onClick={onClickMarkAllAsRead}
            disabled={!numberNotifications || !consoleNotifications.length}
            className={styles.markAllAsReadBtn}
          >
            mark all as read
          </Button>
          <div
            className={styles.closeNotificationIcon}
            onClick={onClickOutside}
          >
            <i className="fa fa-times" />
          </div>
        </Flex>
        <Box className={styles.notificationsContainer}>
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
        </Box>
      </Box>
    </>
  );
};

const notificationSectionConnector = connect(
  mapStateToProps,
  mapDispatchToPropsEmpty
);

type HasuraNotificationsProps = ConnectedProps<
  typeof notificationSectionConnector
>;

const NotificationSection = notificationSectionConnector(HasuraNotifications);

export default NotificationSection;
