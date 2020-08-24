import React from 'react';
import { connect, ConnectedProps } from 'react-redux';

import { Box, Flex, Heading, Text, Badge } from '../UIKit/atoms';
import { ConsoleNotification, NotificationDate } from './ConsoleNotification';
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
import { getReadAllNotificationsState } from './utils';
import { Nullable } from '../Common/utils/tsUtils';
import { mapDispatchToPropsEmpty } from '../Common/utils/reactUtils';
import { HASURA_COLLABORATOR_TOKEN } from '../../constants';

const getDateString = (date: NotificationDate) => {
  if (!date) {
    return '';
  }
  return new Date(date).toLocaleString().split(', ')[0];
};

interface UpdateProps extends ConsoleNotification {
  onClickAction?: (id?: number) => void;
  is_read?: boolean;
}

const Update: React.FC<UpdateProps> = ({
  subject,
  content,
  type,
  is_active = true,
  onClickAction,
  is_read,
  ...props
}) => {
  const [linkClicked, updateLinkClicked] = React.useState(is_read);

  React.useEffect(() => {
    updateLinkClicked(is_read);
  }, [is_read]);

  const onClickLink = () => {
    if (!linkClicked) {
      updateLinkClicked(true);
    }
    if (onClickAction) {
      onClickAction(props.id);
    }
  };

  if (!is_active) {
    return null;
  }
  return (
    <Box
      className={`${styles.updateBox} ${
        !linkClicked ? styles.unread : styles.read
      }`}
      onClick={onClickLink}
    >
      <Flex px="25px" justifyContent="space-between">
        <Flex justifyContent="space-between">
          {type ? <Badge type={type} mr="12px" /> : null}
          <Heading as="h4" color="#1cd3c6" fontSize="16px">
            {subject}
          </Heading>
        </Flex>
        <Text color="grey" fontSize={13} fontWeight="bold">
          {props?.start_date ? getDateString(props.start_date) : null}
        </Text>
      </Flex>
      <Flex
        pt="4px"
        alignItems="center"
        justifyContent="space-between"
        width="100%"
      >
        <Flex px={25} py={2} width="80%">
          <Text fontSize={15} fontWeight="normal">
            {content}
            <br />
            {props.external_link ? (
              <div className={styles.linkContainer}>
                <a
                  href={props.external_link}
                  className={styles.notificationExternalLink}
                  onClick={onClickLink}
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  Click here &rarr;
                </a>
              </div>
            ) : null}
            {props?.children ? props.children : null}
          </Text>
        </Flex>
        <Flex width="20%" alignItems="center" justifyContent="flex-end">
          {!linkClicked ? <div className={styles.yellowDot} /> : null}
        </Flex>
      </Flex>
    </Box>
  );
};

type NotificationProps = {
  data: ConsoleNotification[];
  showVersionUpdate: boolean;
  latestVersion: string;
  optOutCallback: () => void;
  onClickMarkAllAsRead: () => void;
  disableMarkAllAsReadBtn: boolean;
  onClickViewMore: () => void;
  displayViewMore: boolean;
  onClickUpdate: (id?: number) => void;
  previouslyRead?: string | string[];
  fixedVersion: string;
};

type PreReleaseProps = Pick<NotificationProps, 'optOutCallback'>;

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

type VersionUpdateNotificationProps = Pick<
  NotificationProps,
  'latestVersion' | 'optOutCallback'
>;

const VersionUpdateNotification: React.FC<VersionUpdateNotificationProps> = ({
  latestVersion,
  optOutCallback,
}) => {
  const isStableRelease = checkStableVersion(latestVersion);
  return (
    <Update
      subject="New Update Available!"
      type="version update"
      content={`Hey There! There's a new server version ${latestVersion} available.`}
      start_date={Date.now()}
    >
      <a
        href={
          latestVersion
            ? `https://github.com/hasura/graphql-engine/releases/tag/${latestVersion}`
            : 'https://github.com/hasura/graphql-engine/releases'
        }
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

const Notifications = React.forwardRef<HTMLDivElement, NotificationProps>(
  (
    {
      data,
      showVersionUpdate,
      latestVersion,
      optOutCallback,
      disableMarkAllAsReadBtn,
      onClickMarkAllAsRead,
      onClickViewMore,
      displayViewMore,
      onClickUpdate,
      previouslyRead,
      fixedVersion,
    },
    forwardedRef
  ) => (
    <Box
      className={`dropdown-menu ${styles.consoleNotificationPanel}`}
      ref={forwardedRef}
    >
      <Flex
        alignItems="center"
        p={16}
        justifyContent="space-between"
        border="4px solid #f2f2f2"
      >
        <Flex alignItems="center" justifyContent="center">
          <Heading as="h4" color="#000" fontSize="12px" marginLeft="8px">
            Notifications (34)
          </Heading>
        </Flex>
        <Button
          title="Mark all as read"
          onClick={onClickMarkAllAsRead}
          disabled={disableMarkAllAsReadBtn}
          className={`${styles.markAllAsReadBtn}`}
        >
          mark all read
        </Button>
      </Flex>
      <Box className={styles.notificationsContainer}>
        {showVersionUpdate ? (
          <VersionUpdateNotification
            latestVersion={latestVersion}
            optOutCallback={optOutCallback}
          />
        ) : null}
        {fixedVersion ? (
          <VulnerableVersionNotification fixedVersion={fixedVersion} />
        ) : null}
        {data.length &&
          data.map(({ id, ...props }) => (
            <Update
              key={id}
              id={id}
              onClickAction={onClickUpdate}
              is_read={checkIsRead(previouslyRead, id)}
              {...props}
            />
          ))}
        {displayViewMore ? (
          <ViewMoreOptions
            onClickViewMore={onClickViewMore}
            readAll={previouslyRead === 'all'}
          />
        ) : null}
      </Box>
    </Box>
  )
);

type ConsoleOptions = ConsoleState['console_opts'];

const checkVersionUpdate = (
  latestStable: string,
  latestPreRelease: string,
  serverVersion: string,
  console_opts: ConsoleOptions
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

    if (lastUpdateCheckClosed !== latestServerVersionToCheck) {
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
    // TODO: change to design system colors
    <Flex className={`${styles.numBadge} ${showBadge}`}>{display}</Flex>
  );
};

const mapStateToProps = (state: ReduxState) => {
  return {
    consoleNotifications: state.main.consoleNotifications,
    latestPreReleaseServerVersion: state.main.latestPreReleaseServerVersion,
    latestStableServerVersion: state.main.latestStableServerVersion,
    serverVersion: state.main.serverVersion,
    console_opts: state.telemetry.console_opts,
    hasura_uuid: state.telemetry.hasura_uuid,
    dataHeaders: state.tables.dataHeaders,
  };
};

type HasuraNotificationOwnProps = {
  toggleDropDown: (e: React.MouseEvent<HTMLDivElement>) => void;
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
  ...props
}) => {
  const { dispatch } = props;
  const dropDownRef = React.useRef<HTMLDivElement>(null);
  const wrapperRef = React.useRef<HTMLDivElement>(null);
  // NOTE: Multiple useState's here maybe use useReducer for those
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
    userType = dataHeaders[HASURA_COLLABORATOR_TOKEN];
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
    // This is being done to persist the id's that were present at the time of marking it as all read
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

  const onClickShareSection = (
    e: React.MouseEvent<HTMLDivElement, MouseEvent>
  ) => {
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

    toggleDropDown(e);
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

  return (
    <>
      <div
        className={`${styles.shareSection} dropdown-toggle`}
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
      <Notifications
        ref={dropDownRef}
        data={dataShown}
        showVersionUpdate={displayNewVersionNotification}
        latestVersion={latestVersion}
        optOutCallback={optOutCallback}
        onClickMarkAllAsRead={onClickMarkAllAsRead}
        onClickViewMore={onClickViewMore}
        disableMarkAllAsReadBtn={showMarkAllAsRead}
        displayViewMore={toDisplayViewMore}
        onClickUpdate={onClickUpdate}
        previouslyRead={previouslyReadState}
        fixedVersion={fixedVersion}
      />
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
