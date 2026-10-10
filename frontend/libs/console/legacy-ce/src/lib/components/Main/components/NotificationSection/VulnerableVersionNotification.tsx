import React from 'react';
import styles from '../../Main.module.scss';
import Notification from './Notification';

type VulnerableVersionProps = {
  fixedVersion: string;
};

const VulnerableVersionNotification: React.FC<VulnerableVersionProps> = ({
  fixedVersion,
}) => {
  const [startDate] = React.useState(() => Date.now());

  return (
    <Notification
      type="security"
      subject="Security Vulnerability Located!"
      content={`This current server version has a security vulnerability. Please upgrade to ${fixedVersion} immediately.`}
      start_date={startDate}
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
};

export default VulnerableVersionNotification;
