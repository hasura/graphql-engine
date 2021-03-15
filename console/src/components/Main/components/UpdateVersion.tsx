import React from 'react';

import { checkStableVersion } from '../../../helpers/versionUtils';
import { setPreReleaseNotificationOptOutInDB } from '../../../telemetry/Actions';
import { Dispatch } from '../../../types';
import ToolTip from '../../Common/Tooltip/Tooltip';
import styles from '../Main.scss';

type PreReleaseNoteProps = {
  onPreRelNotifOptOut: (e: React.MouseEvent) => void;
};
const PreReleaseNote: React.FC<PreReleaseNoteProps> = ({
  onPreRelNotifOptOut,
}) => {
  return (
    <React.Fragment>
      <span className={styles.middot}> &middot; </span>
      <i>
        This is a pre-release version. Not recommended for production use.
        <span className={styles.middot}> &middot; </span>
        <a href="#" onClick={onPreRelNotifOptOut}>
          Opt out of pre-release notifications
        </a>
        <ToolTip
          message="Only be notified about stable releases"
          placement="top"
        />
      </i>
    </React.Fragment>
  );
};

type UpdateVersionProps = {
  updateNotificationVersion: string;
  dispatch: Dispatch;
  closeUpdateBanner: () => void;
};
export const UpdateVersion: React.FC<UpdateVersionProps> = ({
  updateNotificationVersion,
  dispatch,
  closeUpdateBanner,
}) => {
  const isStableRelease = checkStableVersion(updateNotificationVersion);

  const handlePreRelNotifOptOut = (e: React.MouseEvent) => {
    e.preventDefault();
    e.stopPropagation();

    closeUpdateBanner();

    dispatch(setPreReleaseNotificationOptOutInDB());
  };

  if (!updateNotificationVersion) {
    return null;
  }

  return (
    <div>
      <div className={styles.phantom} />{' '}
      {/* phantom div to prevent overlapping of banner with content. */}
      <div className={styles.updateBannerWrapper}>
        <div className={styles.updateBanner}>
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
          {!isStableRelease && (
            <PreReleaseNote onPreRelNotifOptOut={handlePreRelNotifOptOut} />
          )}
          <span
            className={styles.updateBannerClose}
            onClick={closeUpdateBanner}
          >
            <i className="fa fa-times" />
          </span>
        </div>
      </div>
    </div>
  );
};
