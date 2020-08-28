import React from 'react';
import { connect, ConnectedProps } from 'react-redux';

import { Box, Flex, Heading, Text, Badge } from '../UIKit/atoms';
import {
  ConsoleNotification,
  NotificationDate,
  NotificationScope,
} from './ConsoleNotification';
import styles from './Main.scss';
import useOnClickOutside from '../../hooks/useOnClickOutside';
import { ReduxState, NotificationsState, ConsoleState } from '../../types';
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

const getDateString = (date: NotificationDate) => {
  if (!date) {
    return '';
  }
  return new Date(date).toLocaleString().split(', ')[0];
};

interface UpdateProps extends ConsoleNotification {
  onClickAction?: (id?: number) => void;
  is_read?: boolean;
  onReadCB?: () => void;
  consoleScope: NotificationScope;
}

const Update: React.FC<UpdateProps> = ({
  subject,
  content,
  type,
  is_active = true,
  onClickAction,
  is_read,
  onReadCB,
  ...props
}) => {
  const [linkClicked, updateLinkClicked] = React.useState(is_read);
  const onClickNotification = () => {
    if (!linkClicked) {
      updateLinkClicked(true);
      if (onReadCB) {
        onReadCB();
      }
    }
    if (onClickAction) {
      onClickAction(props.id);
    }
  };

  if (!is_active) {
    return null;
  }

  if (
    !props.scope ||
    (props.scope !== props.consoleScope && props.consoleScope !== 'OSS')
  ) {
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
        !linkClicked ? styles.unread : styles.read
      }`}
      onClick={onClickNotification}
    >
      {!isUpdateNotification ? (
        <div
          className={`${styles.unreadDot} ${linkClicked ? styles.hideDot : ''}`}
        />
      ) : (
        <span
          className={`${styles.unreadStar} ${
            linkClicked ? styles.hideStar : ''
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
}

const VersionUpdateNotification: React.FC<VersionUpdateNotificationProps> = ({
  latestVersion,
  optOutCallback,
}) => {
  const isStableRelease = checkStableVersion(latestVersion);
  const changeLogURL = `https://github.com/hasura/graphql-engine/releases${
    latestVersion ? `/tag/${latestVersion}` : ''
  }`;
  return (
    <Update
      subject="New Update Available!"
      type={isStableRelease ? 'version update' : 'beta update'}
      content={`Hey There! There's a new server version ${latestVersion} available.`}
      start_date={Date.now()}
      consoleScope="OSS"
    >
      <a href={changeLogURL} target="_blank" rel="noopener noreferrer">
        <span>View Changelog</span>
      </a>
      <span className={styles.middot}> &middot; </span>
      <a
        className={styles.updateLink}
        href="https://hasura.io/docs/1.0/graphql/manual/deployment/updating.html"
        target="_blank"
        rel="noopener noreferrer"
      >
        <span>Update Now</span>
      </a>
      {!isStableRelease && <PreReleaseNote optOutCallback={optOutCallback} />}
    </Update>
  );
};

type VulnerableVersionProps = {
  fixedVersion: string;
};

const VulnerableVersionNotification: React.FC<VulnerableVersionProps> = ({
  fixedVersion,
}) => (
  <Update
    type="security"
    subject="Security Vulnerability Located!"
    content={`This current server version has a security vulnerability. Please upgrade to ${fixedVersion} immediately.`}
    start_date={Date.now()}
    consoleScope="OSS"
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
      href="https://hasura.io/docs/1.0/graphql/manual/deployment/updating.html"
      target="_blank"
      rel="noopener noreferrer"
    >
      <span>Update Now</span>
    </a>
  </Update>
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
  if (!prevRead || !id) {
    return false;
  }
  if (prevRead === 'all' || prevRead === 'default' || prevRead === 'error') {
    return true;
  }
  return prevRead.includes(`${id}`);
};

const checkVersionUpdate = (
  latestStable: string,
  latestPreRelease: string,
  serverVersion: string,
  console_opts: ConsoleState['console_opts']
): [boolean, string] => {
  const allowPreReleaseNotifications =
    !console_opts || !console_opts.disablePreReleaseUpdateNotifications;

  let latestServerVersionToCheck = latestStable;
  if (
    allowPreReleaseNotifications &&
    versionGT(latestPreRelease, latestStable)
  ) {
    latestServerVersionToCheck = latestPreRelease;
  }

  // TODO: update with LS utils methods once PR is merged
  const versionCheckKey = 'versionUpdateCheck: lastClosed';

  try {
    const lastUpdateCheckClosed = window.localStorage.getItem(versionCheckKey);
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
  ...props
}) => {
  const { dispatch } = props;
  // eslint-disable-next-line no-underscore-dangle
  const consoleId = window.__env.consoleId;
  const consoleScope = getConsoleScope(serverVersion, consoleId);
  const dropDownRef = React.useRef<HTMLDivElement>(null);
  const wrapperRef = React.useRef<HTMLDivElement>(null);
  const [latestVersion, setLatestVersion] = React.useState(serverVersion);
  const [
    displayNewVersionNotification,
    setDisplayNewVersionNotification,
  ] = React.useState(false);
  const [opened, updateOpenState] = React.useState(false);
  const [numberNotifications, updateNumberNotifications] = React.useState(0);
  const [numDisplayed, updateNumDisplayed] = React.useState(20);
  const [showMarkAllAsRead, toShowMarkAllAsRead] = React.useState(false);
  const [dataShown, updateDataShown] = React.useState<ConsoleNotification[]>(
    []
  );
  const [toDisplayViewMore, updateViewMoreDisplay] = React.useState(true);
  const [previouslyReadState, updatePreviouslyReadState] = React.useState<
    NotificationsState['read']
  >([]);
  const [showBadge, updateShowBadgeState] = React.useState<
    NotificationsState['showBadge']
  >(true);
  const [fixedVersion, updateFixedVersion] = React.useState('');

  let userType = 'admin';

  if (dataHeaders?.[HASURA_COLLABORATOR_TOKEN]) {
    const collabToken = dataHeaders[HASURA_COLLABORATOR_TOKEN];
    userType = getUserType(collabToken);
  }

  React.useEffect(() => {
    if (console_opts?.console_notifications?.[userType]) {
      updatePreviouslyReadState(
        console_opts.console_notifications[userType].read
      );
      updateShowBadgeState(
        console_opts.console_notifications[userType].showBadge
      );
    }
  }, [console_opts?.console_notifications, userType]);

  // for running the version update code on mounting
  React.useEffect(() => {
    if (
      !console_opts ||
      !latestStableServerVersion ||
      !latestPreReleaseServerVersion ||
      !serverVersion
    ) {
      return;
    }

    const [versionUpdateCheck, latestReleasedVersion] = checkVersionUpdate(
      latestStableServerVersion,
      latestPreReleaseServerVersion,
      serverVersion,
      console_opts
    );

    setLatestVersion(latestReleasedVersion);

    if (
      versionUpdateCheck &&
      !console_opts?.disablePreReleaseUpdateNotifications
    ) {
      setDisplayNewVersionNotification(true);
      updateNumberNotifications(num => num++);
      return;
    }

    setDisplayNewVersionNotification(false);
  }, [
    latestPreReleaseServerVersion,
    latestStableServerVersion,
    console_opts,
    serverVersion,
    console_opts?.disablePreReleaseUpdateNotifications,
  ]);

  React.useEffect(() => {
    const vulnerableVersionsMapping: Record<string, string> = {
      'v1.2.0-beta.5': 'v1.2.1',
      'v1.2.0': 'v1.2.1',
    };

    if (Object.keys(vulnerableVersionsMapping).includes(serverVersion)) {
      const fixVersion = vulnerableVersionsMapping[serverVersion];
      updateFixedVersion(fixVersion);
    }
  }, [serverVersion]);

  React.useEffect(() => {
    // once mark all as read is clicked
    if (
      previouslyReadState === 'all' ||
      previouslyReadState === 'default' ||
      previouslyReadState === 'error'
    ) {
      if (displayNewVersionNotification) {
        updateNumberNotifications(1);
      } else {
        updateNumberNotifications(0);
      }
      return;
    }

    let readNumber = consoleNotifications.length;
    if (displayNewVersionNotification) {
      readNumber++;
    }
    if (Array.isArray(previouslyReadState)) {
      readNumber -= previouslyReadState.length;
    }

    updateNumberNotifications(readNumber);
  }, [
    consoleNotifications.length,
    displayNewVersionNotification,
    userType,
    previouslyReadState,
  ]);

  const optOutCallback = () => {
    closeDropDown();
    dispatch(setPreReleaseNotificationOptOutInDB());
  };

  const onClickViewMore = () => {
    const totalNotifs = consoleNotifications.length;
    if (numDisplayed < totalNotifs) {
      const diff = totalNotifs - numDisplayed;
      if (diff > 20) {
        updateNumDisplayed(num => num + 20);
        return;
      }
      updateNumDisplayed(num => num + diff);
    }
  };

  const onClickMarkAllAsRead = () => {
    const readAllState = getReadAllNotificationsState() as NotificationsState;
    dispatch(updateConsoleNotificationsState(readAllState));
    updateNumDisplayed(20);
    // FIXME: this is not really required, since the logic can be changed to filter out all those as
    // having a timestamp prior to the saved time to be marked as read
    window.localStorage.setItem(
      'main:console_notifications',
      JSON.stringify(consoleNotifications)
    );
    // to clear the beta-version update if you mark all as read
    if (!checkStableVersion(latestVersion) && displayNewVersionNotification) {
      optOutCallback();
    }
  };

  const onClickOutside = () => {
    updateOpenState(false);
    closeDropDown();
  };

  useOnClickOutside([dropDownRef, wrapperRef], onClickOutside);

  const onClickShareSection = () => {
    if (showBadge) {
      if (console_opts?.console_notifications) {
        let updatedState = {};
        if (console_opts.console_notifications[userType].date) {
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
      updateNumDisplayed(20);
    }
  }, [opened]);

  const onClickUpdate = (id?: number) => {
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

  React.useEffect(() => {
    if (displayNewVersionNotification && previouslyReadState === 'all') {
      toShowMarkAllAsRead(true);
      return;
    }

    toShowMarkAllAsRead(!numberNotifications);
  }, [numberNotifications, displayNewVersionNotification, previouslyReadState]);

  React.useEffect(() => {
    updateDataShown(consoleNotifications.slice(0, numDisplayed + 1));
  }, [consoleNotifications, numDisplayed]);

  React.useEffect(() => {
    if (
      consoleNotifications.length > 20 &&
      numDisplayed !== consoleNotifications.length
    ) {
      updateViewMoreDisplay(true);
      return;
    }
    updateViewMoreDisplay(false);
  }, [consoleNotifications.length, numDisplayed]);

  const onReadCB = React.useCallback(() => {
    updateNumberNotifications(num => num--);
  }, []);

  return (
    <>
      <div
        className={`${styles.shareSection} ${
          isDropDownOpen ? styles.opened : ''
        } dropdown-toggle`}
        aria-expanded="false"
        onClick={onClickShareSection}
        ref={wrapperRef}
      >
        <i className={`fa fa-bell ${styles.bellIcon}`} />
        <ToReadBadge
          numberNotifications={numberNotifications}
          show={showBadge}
        />
      </div>
      {/* Notifications section */}
      <Box
        className={`dropdown-menu ${styles.consoleNotificationPanel}`}
        ref={dropDownRef}
      >
        <Flex
          alignItems="center"
          p={16}
          justifyContent="space-between"
          border="4px solid #f2f2f2"
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
            disabled={showMarkAllAsRead}
            className={`${styles.markAllAsReadBtn}`}
          >
            mark all read
          </Button>
        </Flex>
        <Box className={styles.notificationsContainer}>
          {displayNewVersionNotification && (
            <VersionUpdateNotification
              latestVersion={latestVersion}
              optOutCallback={optOutCallback}
            />
          )}
          {fixedVersion && (
            <VulnerableVersionNotification fixedVersion={fixedVersion} />
          )}
          {dataShown.length &&
            dataShown.map(({ id, ...otherProps }) => (
              <Update
                key={id}
                id={id}
                onClickAction={onClickUpdate}
                is_read={checkIsRead(previouslyReadState, id)}
                onReadCB={onReadCB}
                consoleScope={consoleScope}
                {...otherProps}
              />
            ))}
          {toDisplayViewMore && (
            <ViewMoreOptions
              onClickViewMore={onClickViewMore}
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
