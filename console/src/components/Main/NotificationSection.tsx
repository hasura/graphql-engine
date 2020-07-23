import React from 'react';
import { connect } from 'react-redux';

import { Box, Flex, Heading, Text, Badge } from '../UIKit/atoms';
import { ConsoleNotification, NotificationDate } from './ConsoleNotification';
import styles from './Main.scss';
import PixelHeart from './images/components/PixelHeart';
import ConsoleLogo from './images/components/ConsoleLogo';
import useOnClickOutside from '../../hooks/useOnClickOutside';
import { ReduxState } from '../../types';
import { TelemetryState } from '../../telemetry/state';
import { versionGT } from '../../helpers/versionUtils';

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
    <Box>
      <Flex height={35} px="25px" pt="5px" justifyContent="space-between">
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
      <Flex borderBottom="1px solid #f7f7f7">
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
};

const VersionUpdateNotification: React.FC<Pick<
  NotificationProps,
  'latestVersion'
>> = ({ latestVersion }) => (
  <Update
    subject="New Update Available!"
    type="version update"
    content={`Hey There! There's a new server version ${latestVersion} available.`}
    start_date={Date.now()}
  >
    <span className={styles.middot}> &middot; </span>
    <a
      href={`https://github.com/hasura/graphql-engine/releases/tag/${latestVersion}`}
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

const Notifications = React.forwardRef<HTMLDivElement, NotificationProps>(
  ({ data, showVersionUpdate, latestVersion }, ref) => (
    <Box
      className={`dropdown-menu ${styles.consoleNotificationPanel}`}
      ref={ref}
    >
      <Flex justifyContent="space-between" px={20} py={3}>
        <Heading as="h2" color="#000" fontSize="20px">
          Latest updates
          <ConsoleLogo className={styles.consoleLogoNotifications} width={20} />
        </Heading>
      </Flex>
      {showVersionUpdate ? (
        <VersionUpdateNotification latestVersion={latestVersion} />
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

  // TODO: update with LS utils PR methods
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

interface Props extends HasuraNotificationProps, StateProps {}

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
      // TODO: add the false case, or the dismiss case
    }
  }, []);

  return (
    <>
      <div
        className={`${styles.shareSection} dropdown-toggle`}
        aria-expanded="false"
        onClick={toggleDropDown}
        ref={wrapperRef}
      >
        <PixelHeart className="img-responsive" width={32} height={20} />
      </div>
      <Notifications
        data={consoleNotifications}
        ref={dropDownRef}
        showVersionUpdate={displayNewVersionNotif}
        latestVersion={latestVersion}
      />
    </>
  );
};

const NotificationSection = connect(mapStateToProps)(HasuraNotifications);

export default NotificationSection;
