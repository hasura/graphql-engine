import React, { FC } from 'react';
import { OverlayTrigger } from 'react-bootstrap';

import KnowMoreLink from '../../../Common/KnowMoreLink/KnowMoreLink';
import styles from '../../../Common/Permissions/PermissionStyles.scss';

interface PermissionSectionHeaderProps {
  title: string | JSX.Element;
  tooltip: JSX.Element;
  knowMoreRef?: string;
  sectionStatus?: string;
}

export const PermissionSectionHeader: FC<PermissionSectionHeaderProps> = props => {
  const { title, tooltip, knowMoreRef, sectionStatus } = props;
  return (
    <div>
      <span>
        <span className={styles.add_mar_right_small}>{title}</span>
        <OverlayTrigger placement="right" overlay={tooltip}>
          <i className="fa fa-question-circle" aria-hidden="true" />
        </OverlayTrigger>
      </span>
      {knowMoreRef && (
        <span
          className={`${styles.add_mar_left_small} ${styles.sectionStatus}`}
        >
          <KnowMoreLink href={knowMoreRef} />
        </span>
      )}
      {sectionStatus && (
        <span className={styles.add_mar_left}>
          - <i className={styles.sectionStatus}>{sectionStatus}</i>
        </span>
      )}
    </div>
  );
};

export default PermissionSectionHeader;
