import React from 'react';
import { Flex } from '@radix-ui/themes';
import styles from '../../Main.module.scss';
import { setLSItem, checkStableVersion } from '@hasura/shared/utils';
import Notification from './Notification';
import { LS_KEYS } from '@hasura/shared/types';
import { IconTooltip } from '@hasura/shared/ui';

type PreReleaseProps = {
  optOutCallback: () => void;
};

const PreReleaseNote: React.FC<PreReleaseProps> = ({ optOutCallback }) => (
  <Flex style={{ paddingTop: 10 }}>
    <i>
      This is a pre-release version. Not recommended for production use.
      <br />
      <a href="#" onClick={optOutCallback}>
        Opt out of pre-release notifications
      </a>
      <IconTooltip
        message="Only be notified about stable releases"
        side="top"
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
  const [startDate] = React.useState(() => Date.now());
  const isStableRelease = checkStableVersion(latestVersion);
  const changeLogURL = `https://github.com/hasura/graphql-engine/releases${
    latestVersion ? `/tag/${latestVersion}` : ''
  }`;

  const handleClick = () => {
    setLSItem(LS_KEYS.versionUpdateCheckLastClosed, latestVersion || '');
    onClick();
  };

  return (
    <Notification
      subject="New Update Available!"
      type={isStableRelease ? 'version update' : 'beta update'}
      content={`Hey There! There's a new server version ${latestVersion} available.`}
      start_date={startDate}
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

export default VersionUpdateNotification;
