import React from 'react';
import { connect } from 'react-redux';

import { Box, Flex, Heading, Text, Badge } from '../UIKit/atoms';
import { ConsoleNotification, NotificationDate } from './ConsoleNotification';
import styles from './Main.scss';
import ConsoleLogo from './images/components/ConsoleLogo';
import useOnClickOutside from '../../hooks/useOnClickOutside';
import { ReduxState, Dispatch } from '../../types';
import { TelemetryState } from '../../telemetry/state';
import { versionGT, checkStableVersion } from '../../helpers/versionUtils';
import ToolTip from '../Common/Tooltip/Tooltip';
import {
  setPreReleaseNotificationOptOutInDB,
  updateConsoleNotificationsInDB,
} from '../../telemetry/Actions';
import Button from '../Common/Button';
import { showErrorNotification } from '../Services/Common/Notification';
import {
  getReadAllNotificationsState,
  fetchConsoleNotifications,
} from './Actions';

const getDateString = (date: NotificationDate) => {
  if (!date) {
    return '';
  }
  return new Date(date).toLocaleString().split(', ')[0];
};

// TODO: Perhaps have to add a close/hide button for some updates

interface ConsoleUpdateProps extends ConsoleNotification {
  children?: React.ReactNode;
}

const Update = React.forwardRef<HTMLDivElement, ConsoleUpdateProps>(
  ({ subject, content, type, is_active = true, ...props }, forwardedRef) => {
    if (!is_active) {
      return null;
    }
    return (
      <Box
        className={styles.updateBox}
        ref={forwardedRef}
        id={props?.id ? `${props.id}` : subject}
      >
        <Flex px="25px" justifyContent="space-between">
          <Flex justifyContent="space-between" bg="white">
            {type ? <Badge type={type} mr="12px" /> : null}
            <Heading as="h4" color="#1cd3c6" fontSize="16px">
              {subject}
            </Heading>
          </Flex>
          <Text color="grey" fontSize={13} fontWeight="bold">
            {props?.start_date ? getDateString(props.start_date) : null}
          </Text>
        </Flex>
        <Flex pt="4px">
          <Text fontSize={15} fontWeight="normal" px={25} py={2}>
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
      </Box>
    );
  }
);

type NotificationProps = {
  data: ConsoleNotification[];
  showVersionUpdate: boolean;
  latestVersion: string;
  optOutCallback: () => void;
  onClickMarkAllAsRead: () => void;
  disableMarkAllAsReadBtn: boolean;
  onClickViewMore: () => void;
  updateRefs: Record<string, React.RefObject<HTMLDivElement>>;
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
      ref={null}
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
  // if all read - no button
  // new > read notifs - See More notifications
  // if current < old - See older notifications
  // if (numberNotifications === 10) {
  //   buttonText = 'View Older Notifications';
  // }
  // TODO: think about the part where we might have to change the text
  return (
    <Button className={styles.viewMoreNotifications} onClick={onClickViewMore}>
      {buttonText} &rarr;
    </Button>
  );
};

const markAllAsRead = (dispatch: Dispatch, uuid: string) => {
  if (!uuid) {
    dispatch(
      showErrorNotification(
        'hasura-uuid absent in the server',
        'You may need to update the server version in order to fix this'
      )
    );
    return;
  }
  try {
    const readAllState = getReadAllNotificationsState();
    dispatch(updateConsoleNotificationsInDB(readAllState));
  } catch (err) {
    dispatch(
      showErrorNotification('Failed to mark all notifications as read', err)
    );
  }
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
      updateRefs,
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
        p={20}
        bg="#e1e1e1"
        justifyContent="space-between"
      >
        <Flex alignItems="center" justifyContent="center">
          <Heading as="h2" color="#000" fontSize="20px">
            Latest updates
          </Heading>
          <ConsoleLogo
            className={styles.consoleLogoNotifications}
            width={24}
            height={24}
          />
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
          data.map(({ subject, content, is_active, ...props }) => (
            <Update
              ref={props?.id ? updateRefs[props.id] : undefined}
              id={props.id}
              key={subject}
              subject={subject}
              content={content}
              type={props.type}
              is_active={is_active}
              {...props}
            />
          ))}
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

const ToReadBadge: React.FC<BadgeViewMoreProps> = ({ numberNotifications }) => {
  if (!numberNotifications || numberNotifications <= 0) {
    return null;
  }

  let display = `${numberNotifications}`;
  if (numberNotifications > 20) {
    display = '20+';
  }

  return (
    // TODO: change to design system colors
    <Flex className={styles.numBadge}>{display}</Flex>
  );
};

function mapStateToProps(state: ReduxState) {
  return {
    consoleNotifications: state.main.consoleNotifications,
    latestPreReleaseServerVersion: state.main.latestPreReleaseServerVersion,
    latestStableServerVersion: state.main.latestStableServerVersion,
    serverVersion: state.main.serverVersion,
    console_opts: state.telemetry.console_opts,
    hasura_uuid: state.telemetry.hasura_uuid,
  };
}

type StateProps = ReturnType<typeof mapStateToProps>;

type HasuraNotificationProps = {
  toggleDropDown: (e: React.MouseEvent<HTMLDivElement>) => void;
  closeDropDown: () => void;
};

interface Props extends HasuraNotificationProps, StateProps {
  dispatch: Dispatch;
}

const HasuraNotifications: React.FC<Props> = ({
  consoleNotifications,
  toggleDropDown,
  closeDropDown,
  ...props
}) => {
  const [displayNewVersionNotif, setDisplayNewVersionNotif] = React.useState(
    false
  );
  const [latestVersion, setLatestVersion] = React.useState(props.serverVersion);
  const dropDownRef = React.useRef<HTMLDivElement>(null);
  const wrapperRef = React.useRef(null);
  const updateRefs = consoleNotifications.reduce(
    (
      acc: Record<string, React.RefObject<HTMLDivElement>>,
      value: ConsoleNotification
    ) => {
      if (value.id) {
        acc[value.id] = React.createRef();
      }
      return acc;
    },
    {}
  );
  const updateRefObjects = Object.values(updateRefs);
  const observerCallback = (entries: IntersectionObserverEntry[]) => {
    // TODO: change this callback to
    // 1. change the read count
    // 2. update the read [] in console_state within the DB
    entries.forEach(entry => {
      if (entry.isIntersecting) {
        console.log(entry.target.id);
      }
    });
  };
  const observer = new IntersectionObserver(observerCallback, {
    root: dropDownRef.current,
    threshold: [1],
  });

  React.useEffect(() => {
    updateRefObjects.forEach(value => {
      if (value.current) {
        observer.observe(value.current);
      }
    });

    // return () => {
    //   updateRefObjects.forEach(value => {
    //     if (value.current) {
    //       observer.unobserve(value.current);
    //     }
    //   });
    // };
  }, [updateRefObjects, updateRefs, observer]);

  React.useEffect(() => {
    const [versionUpdateCheck, latestReleasedVersion] = checkVersionUpdate(
      props.latestStableServerVersion,
      props.latestPreReleaseServerVersion,
      props.serverVersion,
      props.console_opts
    );

    setLatestVersion(latestReleasedVersion);

    if (versionUpdateCheck) {
      setDisplayNewVersionNotif(true);
      return;
    }

    setDisplayNewVersionNotif(false);
  }, [props]);

  const optOutCallback = () => {
    props.dispatch(setPreReleaseNotificationOptOutInDB());
  };

  const onClickViewMore = () => {
    props.dispatch(fetchConsoleNotifications());
  };

  const onClickMarkAllAsRead = () => {
    markAllAsRead(props.dispatch, props.hasura_uuid);
  };

  let numberNotifications =
    consoleNotifications.length + (displayNewVersionNotif ? 1 : 0);
  // TODO: handle read logic here and send the appropriate number
  if (props.console_opts?.console_notifications) {
    const readValue = props.console_opts.console_notifications.read;
    if (
      readValue === 'all' ||
      readValue === 'default' ||
      readValue === 'error' ||
      readValue === []
    ) {
      // after `Mark all as read` or no new notifications case
      numberNotifications = 0;
    }
  }

  useOnClickOutside([dropDownRef, wrapperRef], closeDropDown);

  return (
    <>
      <div
        className={`${styles.shareSection} dropdown-toggle`}
        aria-expanded="false"
        onClick={toggleDropDown}
        ref={wrapperRef}
      >
        <ConsoleLogo width={25} height={25} />
        <ToReadBadge numberNotifications={numberNotifications} />
      </div>
      <Notifications
        ref={dropDownRef}
        data={consoleNotifications}
        showVersionUpdate={displayNewVersionNotif}
        latestVersion={latestVersion}
        optOutCallback={optOutCallback}
        onClickMarkAllAsRead={onClickMarkAllAsRead}
        onClickViewMore={onClickViewMore}
        disableMarkAllAsReadBtn={!numberNotifications}
        updateRefs={updateRefs}
      />
    </>
  );
};

const NotificationSection = connect(mapStateToProps)(HasuraNotifications);

export default NotificationSection;
