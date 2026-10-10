import React, { useEffect, useState } from 'react';
import { FaTimes } from 'react-icons/fa';
import styles from '../Main.module.scss';
import useCatalogState from '../../../telemetry/useCatalogState';
import {
  getLSItem,
  setLSItem,
  checkStableVersion,
  versionGT,
} from '@hasura/shared/utils';
import { useAppContext } from '@hasura/shared/context';
import { ConsoleState } from '@hasura/metadata/api';
import { LS_KEYS } from '@hasura/shared/types';
import { IconTooltip } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

type PreReleaseNoteProps = {
  onPreRelNotifOptOut: (e: React.MouseEvent) => void;
};

const PreReleaseNote: React.FC<PreReleaseNoteProps> = ({
  onPreRelNotifOptOut,
}) => {
  return (
    <Flex align="center" justify="center" gap="1">
      <span className={styles.middot}> &middot; </span>
      <i>
        This is a pre-release version. Not recommended for production use.
        <span className={styles.middot}> &middot; </span>
        <a href="#" onClick={onPreRelNotifOptOut}>
          Opt out of pre-release notifications
        </a>
      </i>
      <IconTooltip
        message="Only be notified about stable releases"
        side="top"
      />
    </Flex>
  );
};

type UpdateVersionProps = {
  consoleState: ConsoleState;
};

export const UpdateVersion: React.FC<UpdateVersionProps> = ({
  consoleState,
}) => {
  const { envVars } = useAppContext();
  const { serverVersion, latestServerVersion } = useAppContext();
  const [updateNotificationVersion, setUpdateNotificationVersion] = useState<
    string | null
  >(null);
  const { setPreReleaseNotificationOptOut } = useCatalogState();

  const setShowUpdateNotification = () => {
    const allowPreReleaseNotifications =
      !consoleState.disablePreReleaseUpdateNotifications;

    const latestServerVersionToCheck =
      allowPreReleaseNotifications &&
      versionGT(latestServerVersion.prerelease, latestServerVersion.latest)
        ? latestServerVersion.prerelease
        : latestServerVersion.latest;

    try {
      const lastUpdateCheckClosed = getLSItem(
        LS_KEYS.versionUpdateCheckLastClosed,
      );

      if (lastUpdateCheckClosed !== latestServerVersionToCheck) {
        const isUpdateAvailable = versionGT(
          latestServerVersionToCheck,
          serverVersion,
        );

        if (isUpdateAvailable) {
          setUpdateNotificationVersion(latestServerVersionToCheck);
        }
      }
    } catch (e) {
      console.error(e);
    }
  };

  useEffect(() => {
    setShowUpdateNotification();
  }, []);

  const closeUpdateBanner = () => {
    if (updateNotificationVersion) {
      setLSItem(
        LS_KEYS.versionUpdateCheckLastClosed,
        updateNotificationVersion,
      );
    }

    setUpdateNotificationVersion(null);
  };

  const handlePreRelNotifOptOut = (e: React.MouseEvent) => {
    e.preventDefault();
    e.stopPropagation();
    closeUpdateBanner();
    setPreReleaseNotificationOptOut();
  };

  if (!updateNotificationVersion || envVars.consoleType !== 'oss') {
    return null;
  }

  const isStableRelease = updateNotificationVersion
    ? checkStableVersion(updateNotificationVersion)
    : false;

  return (
    <div>
      <div className={styles.phantom} />{' '}
      {/* phantom div to prevent overlapping of banner with content. */}
      <div className={styles.updateBannerWrapper}>
        <div className={styles.updateBanner}>
          <div>
            <div>
              <span> Hey there! A new server version </span>
              <span className={styles.versionUpdateText}>
                {' '}
                {updateNotificationVersion}
              </span>
              <span> is available </span>
              <span className={styles.middot}> &middot; </span>
              <a
                href={`https://github.com/hasura/graphql-engine/releases/tag/${updateNotificationVersion}`}
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
            </div>
            {!isStableRelease && (
              <PreReleaseNote onPreRelNotifOptOut={handlePreRelNotifOptOut} />
            )}
          </div>
          <span
            className={styles.updateBannerClose}
            onClick={closeUpdateBanner}
          >
            <FaTimes />
          </span>
        </div>
      </div>
    </div>
  );
};
