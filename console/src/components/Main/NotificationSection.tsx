import React from 'react';
import { connect, ConnectedProps } from 'react-redux';

import { Box, Flex, Heading, Text, Badge } from '../UIKit/atoms';
import { ConsoleNotification, NotificationDate } from './ConsoleNotification';
import styles from './Main.scss';
import ConsoleLogo from './images/components/ConsoleLogo';
import useOnClickOutside from '../../hooks/useOnClickOutside';
import { ReduxState, Dispatch, Thunk } from '../../types';
import {
  TelemetryState,
  TelemetryConsoleNotification,
} from '../../telemetry/state';
import { versionGT, checkStableVersion } from '../../helpers/versionUtils';
import ToolTip from '../Common/Tooltip/Tooltip';
import {
  setPreReleaseNotificationOptOutInDB,
  updateConsoleNotificationsInDB,
} from '../../telemetry/Actions';
import Button from '../Common/Button';
import {
  getReadAllNotificationsState,
  fetchConsoleNotifications,
} from './Actions';
import { Nullable } from '../Common/utils/tsUtils';
import { mapDispatchToPropsEmpty } from '../Common/utils/reactUtils';

const getDateString = (date: NotificationDate) => {
  if (!date) {
    return '';
  }
  return new Date(date).toLocaleString().split(', ')[0];
};

// TODO: Perhaps have to add a close/hide button for some updates

const Update: React.FC<ConsoleNotification> = ({
  subject,
  content,
  type,
  is_active = true,
  ...props
}) => {
  if (!is_active) {
    return null;
  }
  return (
    <Box className={`${styles.updateBox} ${styles.unread}`}>
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
                >
                  Click here &rarr;
                </a>
              </div>
            ) : null}
            {props?.children ? props.children : null}
          </Text>
        </Flex>
        <Flex width="20%" alignItems="center" justifyContent="flex-end">
          <div className={styles.yellowDot} />
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

interface BadgeViewMoreProps {
  numberNotifications: number;
}

interface ViewMoreProps extends BadgeViewMoreProps {
  onClickViewMore: () => void;
}

const ViewMoreOptions: React.FC<ViewMoreProps> = ({
  numberNotifications,
  onClickViewMore,
}) => {
  const buttonText =
    numberNotifications > 20
      ? 'View older notifications'
      : 'View more notifications';
  // new > read notifs - See More notifications
  // if current < old - See older notifications
  // TODO: think about the part where we might have to change the text
  return (
    <Button className={styles.viewMoreNotifications} onClick={onClickViewMore}>
      {buttonText} &rarr;
    </Button>
  );
};

const markAllAsRead = (dispatch: Dispatch, uuid: string) => {
  const readAllState = getReadAllNotificationsState() as TelemetryConsoleNotification;
  dispatch(updateConsoleNotificationsInDB(readAllState));
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
    },
    forwardedRef
  ) => (
    <Box
      className={`dropdown-menu ${styles.consoleNotificationPanel}`}
      ref={forwardedRef}
    >
      {/* TODO: Use style system colors here */}
      <Flex
        alignItems="center"
        p={16}
        bg="#f2f2f2"
        justifyContent="space-between"
      >
        <Flex alignItems="center" justifyContent="center">
          <ConsoleLogo
            className={styles.consoleLogoNotifications}
            width={18}
            height={18}
          />
          <Heading as="h4" color="#000" fontSize="16px" marginLeft="8px">
            Latest updates
          </Heading>
        </Flex>
        <Button
          title="Mark all as read"
          onClick={onClickMarkAllAsRead}
          // TODO: this can change to a state dependent on when all we show the `View more button`
          disabled={disableMarkAllAsReadBtn}
        >
          Mark all as read
        </Button>
      </Flex>
      <Box className={styles.notificationsContainer}>
        {showVersionUpdate ? (
          <VersionUpdateNotification
            latestVersion={latestVersion}
            optOutCallback={optOutCallback}
          />
        ) : null}
        {data.length &&
          data.map(({ id, ...props }) => <Update key={id} {...props} />)}
        <ViewMoreOptions
          numberNotifications={data.length}
          onClickViewMore={onClickViewMore}
        />
      </Box>
    </Box>
  )
);

type ConsoleOptions = TelemetryState['console_opts'];

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

interface ToReadBadgeProps extends BadgeViewMoreProps {
  show: Nullable<boolean>;
}

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
  };
};

type StateProps = ReturnType<typeof mapStateToProps>;

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
  ...props
}) => {
  const { dispatch } = props;
  const [
    displayNewVersionNotification,
    setDisplayNewVersionNotification,
  ] = React.useState(false);
  const [latestVersion, setLatestVersion] = React.useState(serverVersion);
  const dropDownRef = React.useRef<HTMLDivElement>(null);
  const wrapperRef = React.useRef(null);
  // TODO: the number should become zero once it is opened for the first time
  // we can store `opened` in local storage
  const [numberNotifications, updateNumberNotifications] = React.useState(0);
  const showBadge = console_opts?.console_notifications?.showBadge;

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

    if (versionUpdateCheck) {
      setDisplayNewVersionNotification(true);
      return;
    }

    setDisplayNewVersionNotification(false);
  }, [
    latestPreReleaseServerVersion,
    latestStableServerVersion,
    console_opts,
    serverVersion,
  ]);

  React.useEffect(() => {
    const readNotifications = console_opts?.console_notifications?.read;

    // once mark all as read is clicked
    if (
      readNotifications === 'all' ||
      readNotifications === 'default' ||
      readNotifications === 'error'
    ) {
      updateNumberNotifications(0);
      return;
    }

    let readNumber = consoleNotifications.length;
    if (displayNewVersionNotification) {
      readNumber++;
    }
    if (Array.isArray(readNotifications)) {
      readNumber -= readNotifications.length;
    }

    updateNumberNotifications(readNumber);
  }, [
    consoleNotifications.length,
    console_opts?.console_notifications?.read,
    displayNewVersionNotification,
  ]);

  const optOutCallback = () => {
    dispatch(setPreReleaseNotificationOptOutInDB());
  };

  const onClickViewMore = () => {
    // TODO: to change the
    dispatch(fetchConsoleNotifications());
  };

  const onClickMarkAllAsRead = () => {
    const readAllState = getReadAllNotificationsState() as TelemetryConsoleNotification;
    dispatch(updateConsoleNotificationsInDB(readAllState));
    // updateNumberNotifications(0);
  };

  useOnClickOutside([dropDownRef, wrapperRef], closeDropDown);

  return (
    <>
      <div
        className={`${styles.shareSection} dropdown-toggle`}
        aria-expanded="false"
        onClick={toggleDropDown}
        ref={wrapperRef}
      >
        <i className="fa fa-bell" />
        <ToReadBadge
          numberNotifications={numberNotifications}
          show={showBadge}
        />
      </div>
      <Notifications
        ref={dropDownRef}
        data={consoleNotifications}
        showVersionUpdate={displayNewVersionNotification}
        latestVersion={latestVersion}
        optOutCallback={optOutCallback}
        onClickMarkAllAsRead={onClickMarkAllAsRead}
        onClickViewMore={onClickViewMore}
        disableMarkAllAsReadBtn={!numberNotifications}
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
