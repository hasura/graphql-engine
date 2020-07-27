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
import { setPreReleaseNotificationOptOutInDB } from '../../telemetry/Actions';
import Button from '../Common/Button';

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
    <Box className={styles.updateBox}>
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
          {props.children}
        </Text>
      </Flex>
    </Box>
  );
};

type NotificationProps = {
  data: ConsoleNotification[];
  showVersionUpdate: boolean;
  latestVersion: string;
  optOutCallback: () => void;
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

const Notifications = React.forwardRef<HTMLDivElement, NotificationProps>(
  ({ data, showVersionUpdate, latestVersion, optOutCallback }, ref) => (
    <Box
      className={`dropdown-menu ${styles.consoleNotificationPanel}`}
      ref={ref}
    >
      {/* TODO: Use style system colors here */}
      <Flex alignItems="center" p={20} bg="#e1e1e1" justifyContent="space-between">
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
        {/* TODO: add mark all as read functionality -> clear the badge, clear db value and..... also this might be a little too large? */}
        <Button title="Mark all as read" onClick={() => { console.log('Read all!') }}>
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
              key={subject}
              subject={subject}
              content={content}
              type={props.type}
              is_active={is_active}
              {...props}
            />
          ))}
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

type ToReadBadgeProps = {
  numberNotifications: number;
};

// TODO: add the read part as well (perhaps at the level of HasuraNotifications)
const ToReadBadge: React.FC<ToReadBadgeProps> = ({ numberNotifications }) => {
  if (!numberNotifications || numberNotifications < 0) {
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
  const dropDownRef = React.useRef(null);
  const wrapperRef = React.useRef(null);
  useOnClickOutside([dropDownRef, wrapperRef], closeDropDown);

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

  const numberNotifications =
    consoleNotifications.length + (displayNewVersionNotif ? 1 : 0);
  // TODO: handle read logic here and send the appropriate number

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
        data={consoleNotifications}
        ref={dropDownRef}
        showVersionUpdate={displayNewVersionNotif}
        latestVersion={latestVersion}
        optOutCallback={optOutCallback}
      />
    </>
  );
};

const NotificationSection = connect(mapStateToProps)(HasuraNotifications);

export default NotificationSection;
