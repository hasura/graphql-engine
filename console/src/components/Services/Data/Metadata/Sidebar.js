import React from 'react';
import LeftContainer from '../../../Common/Layout/LeftContainer/LeftContainer';
import { Link } from 'react-router';
import styles from '../../../Common/TableCommon/Table.scss';
import CheckIcon from '../../../Common/Icons/Check';
import CrossIcon from '../../../Common/Icons/Cross';

const Sidebar = ({
  location,
  supportMetadata,
  supportInconsistentMetadata,
  metadata,
}) => {
  const currentLocation = location.pathname;
  const sections = [];
  if (supportMetadata) {
    sections.push(
      <li
        role="presentation"
        className={
          currentLocation.includes('metadata/actions') ? styles.active : ''
        }
      >
        <Link
          className={styles.linkBorder}
          to={'/metadata/actions'}
          data-test="metadata-actions-link"
        >
          Actions
        </Link>
      </li>
    );
  }
  if (supportInconsistentMetadata) {
    let consistentIcon = <CheckIcon className={styles.add_mar_left_small} />;
    if (metadata.inconsistentObjects.length > 0) {
      consistentIcon = <CrossIcon className={styles.add_mar_left_small} />;
    }
    sections.push(
      <li
        role="presentation"
        className={
          currentLocation.includes('metadata/status') ? styles.active : ''
        }
      >
        <Link
          className={styles.linkBorder}
          to={'/metadata/status'}
          data-test="metadata-status-link"
        >
          <div className={styles.display_flex}>
            Status
            {consistentIcon}
          </div>
        </Link>
      </li>
    );
  }
  const content = <ul>{sections}</ul>;
  return <LeftContainer>{content}</LeftContainer>;
};

export default Sidebar;
